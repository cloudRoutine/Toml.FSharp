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
let ws = isAnyOf [' ';'\t']
let toml_space       : Parser<_> = satisfy ws
let toml_spaces      : Parser<_> = manySatisfy ws
let skip_toml_spaces : Parser<_> = skipManySatisfy ws

let tspc       = toml_space
let tspcs      = toml_spaces
let skip_tspcs = skip_toml_spaces
    //choice [attempt (skip_toml_spaces .>> skipUnicodeNewline .>> skip_toml_spaces);
      //      skip_toml_spaces]
let skipToEOF :Parser<_>= skipManyTill skipAnyChar eof

(*| Comment/LineEnd Parsers |*)

let skipComment     : Parser<_> = skipChar '#' >>. skipRestOfLine  true
let tskipRestOfLine : Parser<_> = skipComment <|>  skipRestOfLine  true
let tskipper = skipMany (choice [skip_tspcs; tskipRestOfLine])

let spcblock: Parser<_> = 
    skipMany(choice [   skip_tspcs .>> skipUnicodeNewline 
                        skip_tspcs .>> tskipRestOfLine      ])


(*| Punctuation Parsers |*)


let ``.``   : Parser<_> = pchar '.' .>> skip_tspcs 
let ``,``   : Parser<_> = pchar ',' .>> skip_tspcs   

let ``[``   : Parser<_> = pchar '[' .>> skip_tspcs              
let ``]``   : Parser<_> = pchar ']' .>> skip_tspcs   
let ``{``   : Parser<_> = pchar '{' .>> skip_tspcs   
let ``}``   : Parser<_> = pchar '}' .>> skip_tspcs    
let ``[[``  : Parser<_> = pstring "[[" .>> skip_tspcs  
let ``]]``  : Parser<_> = pstring "]]" .>> skip_tspcs  
let ``"``   : Parser<_> = pchar '"'
let ``'``   : Parser<_> = pchar '\''
let ``"""`` : Parser<_> = pstring "\"\"\""
let ``'''`` : Parser<_> = pstring "\'\'\'"

let prevCharNot = previousCharSatisfiesNot
let ``|"|``   : Parser<_> = prevCharNot((=)'\\') >>. pchar '"'
let ``|'|``   : Parser<_> = prevCharNot((=)'\\') >>. pchar '\''
let ``|"""|`` : Parser<_> = prevCharNot((=)'\\') >>. pstring "\"\"\""
let ``|'''|`` : Parser<_> = prevCharNot((=)'\\') >>. pstring "\'\'\'"
let skipEqs   : Parser<_> = skipChar '=' >>. skip_tspcs


(*| String Parsers |*)

// TODO - fully implement string spec
// probably need to make a low level string parser
let psingle_string    : Parser<_> = 
    between ``|"|`` ``|"|`` 
        (manyChars ((previousCharSatisfies ((=)'\\')>>.``"``)<|> satisfy ((<>)'"'))) 

let pmult_string      : Parser<_> = (between ``|"""|`` ``|"""|`` (manyChars anyChar) )
let psingle_litstring : Parser<_> = (between ``|'|`` ``|'|`` (manySatisfy ((<>)'\'')))
let pmult_litstring   : Parser<_> = (between ``|'''|`` ``|'''|`` (manyChars anyChar) )

let toml_string : Parser<_> =
    (choice [ psingle_string; pmult_string; psingle_litstring; pmult_litstring ]
        |>> Value.String) .>> skip_tspcs


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
let toml_key       : Parser<_> = (choice [pBareKey; pQuoteKey ]).>> skip_tspcs

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

let parr : Parser<_> = 
    let ``[``:Parser<_> = (attempt (``[`` .>> unicodeNewline .>> skip_tspcs)) <|> ``[`` 
    let ``]``:Parser<_> = (attempt (skip_tspcs .>> unicodeNewline .>> skip_tspcs >>. ``]``)) <|> ``]`` 
    let ``,``:Parser<_> = (attempt (``,`` .>> unicodeNewline .>> skip_tspcs)) <|> ``,`` 
    between  
        ``[`` ``]`` (sepBy toml_value ``,``) |>> Value.Array

pArrayImpl := parr .>> skip_tspcs
    
    

pITblImpl :=
    (between ``{`` ``}`` (sepBy pKVP ``,``)
    |>> fun items ->
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        Value.InlineTable tbl)
        .>> skip_tspcs

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

pValueImpl := value_parser .>> skip_tspcs

        
(*| Toplevel Parsers |*)

let toml_item : Parser<_> = toml_key .>>. (skipEqs >>. toml_value) .>> tskipRestOfLine

let toml_toplevel, private pToplevelImpl = createParserForwardedToRef ()


let pTable : Parser<_> =
    ((pTableKey .>> tskipRestOfLine) .>>. (many (toml_toplevel))
    |>> fun (ks,items) -> 
        let tk = List.last ks
        let tbl:(_,_) table = table<_,_> ()
        //List.iter (fun x -> printfn "%A" x) items
        List.iter (tbl.Add) items
        tk,Value.Table tbl )
        .>> choice[attempt skipUnicodeNewline; attempt skip_tspcs]
//        Value.Table tbl )


let pTableArray : Parser<_> =
    ((pTableArrayKey .>> tskipRestOfLine) .>>. (many toml_toplevel)
    |>> fun (ks,tbls) -> 
        let ak = List.last ks
        let arr =
            tbls |> List.map (fun (key,value) -> 
            let tbl:(_,_) table = table<_,_> ()
            tbl.Add(key,value)
            tbl
        ) 
        ak, Value.TableArray arr)
    //    .>> skip_tspcs
    
let inline print str x = printfn "%s%A" str x; x

let private toplevel_parser (stream: CharStream<_>) =
    match stream.Peek () with
    | '#'  -> (skipComment >>. toml_toplevel .>> skip_tspcs) stream
    | ' '
    | '\t' -> (skip_tspcs >>. toml_toplevel .>> skip_tspcs) stream
    //| '\n' -> (skipUnicodeNewline >>. toml_toplevel .>> skip_tspcs) stream
    | '['  -> 
        if stream.Peek2() = TwoChars('[','[') then (pTableArray.>> skip_tspcs) stream else
        (pTable.>> skip_tspcs ) stream
    | c when (isDigit|?|isLetter|?|isAnyOf['"';'\'']) c ->
        (toml_item .>> skip_tspcs) stream
    | c    -> 
//        printfn "failchar = %c" c
//        printfn "Line - %d | Col - %d | Index - %d" 
//            stream.Position.Line stream.Position.Column stream.Position.Index
        Reply (Error, expected "A TOML table, array of tables, or a key value pair")

pToplevelImpl := 
   // (skip_tspcs >>. skipUnicodeNewline >>. skip_tspcs) 
   // >>. 
    toplevel_parser 
    //.>> skip_tspcs

let parse_toml : Parser<_> = 
    (many1 (skip_tspcs >>. toml_toplevel .>> skip_tspcs)
    |>> fun items -> 
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        tbl) .>> (skipUnicodeNewline <|> skipToEOF)
(* 
    ---- NOTES ----
    
    TODO - Proper checks for `_` rules and `.` count
    
    Does [<MethodImpl (MethodImplOptions.AggressiveInlining)>] do anything worthwhile?

*)
