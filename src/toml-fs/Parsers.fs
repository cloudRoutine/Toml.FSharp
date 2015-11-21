module TomlFs.Parsers
#nowarn "62"
open System
open System.Collections.Generic
open FParsec
open FParsec.Primitives
open TomlFs.AST

(*| Helpers |*)

/// Compose predicates with `&&`
let inline (|&|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x && pred2 x

/// Compose predicates with `||`
let inline (|?|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x || pred2 x
[<RequireQualifiedAccess>]
module  List =
    let inline last ls =
        let rec loop ls =
            match ls with
            | x::[] -> x | _::tl -> loop tl | [] -> failwith "empty list has no last member"
        loop ls

type UserState = unit
type Parser<'t> = Parser<'t,UserState>


(*| Whitespace Parsers |*)

/// toml approved whitespace is ' ' or '\t'
let toml_space       : Parser<_> = satisfy ((=)' '|?|(=)'\t')
let toml_spaces      : Parser<_> = manySatisfy ((=)' '|?|(=)'\t')
let skip_toml_spaces : Parser<_> = skipManySatisfy ((=)' '|?|(=)'\t')

let tspc       = toml_space
let tspcs      = toml_spaces
let skip_tspcs = skip_toml_spaces


(*| Comment/LineEnd Parsers |*)
let skipComment     : Parser<_> = skipChar '#' >>. skipRestOfLine  true
let tskipRestOfLine : Parser<_> = skipComment <|>  skipRestOfLine  true
let tskipper = skipMany (choice [skip_tspcs; tskipRestOfLine])

let spcblock: Parser<_> = 
    skipMany(choice [   skip_tspcs .>> skipUnicodeNewline 
                        skip_tspcs .>> tskipRestOfLine      ])

(*| Punctuation Parsers |*)

let ``.``   : Parser<_> = pchar '.'
let ``,``   : Parser<_> = attempt (skip_tspcs >>. pchar ',' .>> skip_tspcs)  

let ``[``   : Parser<_> = skip_tspcs >>. pchar '['    .>> skip_tspcs           
let ``]``   : Parser<_> = skip_tspcs >>. pchar ']'    .>> skip_tspcs
let ``{``   : Parser<_> = skip_tspcs >>. pchar '{'    .>> skip_tspcs
let ``}``   : Parser<_> = skip_tspcs >>. pchar '}'    .>> skip_tspcs 
let ``[[``  : Parser<_> = skip_tspcs >>. pstring "[[" .>> skip_tspcs  
let ``]]``  : Parser<_> = skip_tspcs >>. pstring "]]" .>> skip_tspcs  
let ``"``   : Parser<_> = pchar '"'
let ``'``   : Parser<_> = pchar '\''
let ``"""`` : Parser<_> = pstring "\"\"\""
let ``'''`` : Parser<_> = pstring "\'\'\'"
let skipEqs : Parser<_> = skip_tspcs >>. skipChar '=' >>. skip_tspcs




(*| String Parsers |*)

let psingle_string    : Parser<_> = between ``"`` ``"`` (manySatisfy ((<>)'"'))
let pmult_string      : Parser<_> = between ``"""`` ``"""`` (manyChars anyChar)
let psingle_litstring : Parser<_> = between ``'`` ``'`` (manySatisfy ((<>)'\''))
let pmult_litstring   : Parser<_> = between ``'''`` ``'''`` (manyChars anyChar)
let pString_toml      : Parser<_> = psingle_string <|> pmult_string

let toml_string : Parser<_> =
    choice [ psingle_string; pmult_string; psingle_litstring; pmult_litstring ]
    |>> Value.String


(*| Numeric Parsers |*)

let pInt64_toml : Parser<_> = 
    followedByL (satisfy ((<>)'0')) "TOML ints cannot begin with leading 0s"
    >>. many1Chars (skipChar '_' >>. digit <|> digit)
    .>> notFollowedByL ``.`` "TOML ints cannot contain `.`"
    |>> int64


let pFloat_toml : Parser<_> = 
    let floatChar = satisfy (isDigit|?|isAnyOf['e';'E';'+';'-';'.'])
    followedByL (satisfy ((<>)'0')) "TOML floats cannot begin with leading 0s"  
    >>. many1Chars (skipChar '_' >>. floatChar <|> floatChar)
    |>> float

let private toDateTime str =
    let mutable dt = Unchecked.defaultof<DateTime>
    match DateTime.TryParse (str, &dt) with
    | false -> failwithf "failed parsing into DateTime - %s" str
    | true  -> dt

let pDateTime_toml : Parser<_> = manySatisfy (isDigit|?|isAnyOf['T';':';'.';'-';'Z']) |>> toDateTime

let toml_int      = pInt64_toml    |>> Value.Int
let toml_float    = pFloat_toml    |>> Value.Float
let toml_datetime = pDateTime_toml |>> Value.DateTime

(*| Simple Value Parsers |*)

let pBool_toml : Parser<_> = (pstring "false" >>% false) <|> (pstring "true" >>% true)

let toml_bool = pBool_toml |>> Value.Bool


(*| Key Parsers |*)

// key formats
let pBareKey       : Parser<_> = many1Satisfy (isDigit|?|isLetter|?|isAnyOf['_';'-']) 
let pQuoteKey      : Parser<_> = between ``"`` ``"`` (many1Chars anyChar) 

// key in a collection
let toml_key       : Parser<_> = choice [pBareKey; pQuoteKey ]

// toplevel keys
let pTableKey      : Parser<_> = between ``[`` ``]`` (sepBy toml_key ``.``)
let pTableArrayKey : Parser<_> = between ``[[`` ``]]`` (sepBy toml_key ``.``)

(*  Collection Parsers *)

// Forward declaration to allow mutually recursive 
// parsers between arrays and inline tables
let toml_value,       private pValueImpl  = createParserForwardedToRef ()
let toml_array,       private pArrayImpl  = createParserForwardedToRef ()
let toml_inlineTable, private pITblImpl   = createParserForwardedToRef ()

let pKVP : Parser<_>  =  
    tspcs >>. toml_key .>>. (skipEqs >>. toml_value)
 

pArrayImpl := between  ``[`` ``]`` (sepBy toml_value ``,``) |>> Value.Array

pITblImpl :=
    between ``{`` ``}`` (sepBy pKVP ``,``)
    |>> fun items ->
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        Value.InlineTable tbl


// low level parser implementation for simple toml values
/// parses strings, ints, floats, bools, datetimes, inline tables, and arrays
let private value_parser (stream: CharStream<_>) =
    match stream.Peek() with
    | '{' -> toml_inlineTable   stream
    | '[' -> toml_array         stream
    | '"' | '\'' -> toml_string stream
    | 't' | 'f'  -> toml_bool   stream
    | c when isDigit c ->
        // A Date time will always have a `-` at this position e.g. 
        if stream.Peek 4 = '-' then toml_datetime stream else
        let state = stream.State
        let reply = toml_int stream
        if reply.Status = Ok then reply else
        // if parsing for an int fails, backtrack and try a float
        stream.BacktrackTo state
        let reply = toml_float stream
        if reply.Status = Ok then reply else
        stream.BacktrackTo state
        Reply (Error, ErrorMessageList.Merge (expected "some kind of TOML value", reply.Error))
    | _ -> Reply (Error, expected "some kind of TOML value")

pValueImpl := value_parser

        
(*| Toplevel Parsers |*)

let toml_item : Parser<_> = toml_key .>>. (skipEqs >>. toml_value)

let toml_toplevel, private pToplevelImpl      = createParserForwardedToRef ()
//let pTableArray_toml,private pTableArrayImpl = createParserForwardedToRef ()

let pTable : Parser<_> =
    //manyTill (toml_item .>> tskipRestOfLine) eof
    (((pTableKey |>> fun ks -> List.last ks).>> tskipRestOfLine) 
    .>>. 
     (many1 (toml_toplevel .>> tskipRestOfLine))
    |>> fun (tk,items) -> 
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        tk,tbl )
//        Value.Table tbl )


let pTableArray : Parser<_> =
    ((pTableArrayKey .>> tskipRestOfLine) 
        |>> fun ks -> List.last ks
    //manyTill (toml_item .>> tskipRestOfLine) eof
        .>>. 
     (many1 (toml_toplevel .>> tskipRestOfLine))
    |>> fun (ak,items) -> 
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        ak,Value.Array ([Value.InlineTable tbl])
    )


let private toplevel_parser (stream: CharStream<_>) =
    match stream.Peek() with
    | '{' -> pTableArray   stream
    | '[' -> (pTable |>> fun (k,tbl) -> k,Value.Table(tbl))       stream
    | c when (isDigit|?|isLetter|?|isAnyOf['"';'\'']) c ->
        toml_item stream
    | _ -> Reply (Error, expected "A TOML table, array of tables, or a key value pair")

pToplevelImpl := toplevel_parser

let parse_toml : Parser<_> = 
    //(skip_tspcs >>. many1 (toml_toplevel .>> tskipRestOfLine ))
    spcblock >>. (many1 (toml_toplevel .>> (many1 tskipRestOfLine )))
    |>> fun items -> 
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        tbl
(* 
    ---- NOTES ----
    
    TODO - Proper checks for `_` rules and `.` count
    
    Does [<MethodImpl (MethodImplOptions.AggressiveInlining)>] do anything worthwhile?

*)
