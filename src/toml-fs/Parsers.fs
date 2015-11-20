module TomlFs.Parsers
#nowarn "62"
open System
open System.Collections.Generic
open FParsec
open FParsec.Primitives
open TomlFs.AST

/// Compose predicates with `&&`
let inline (|&|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x && pred2 x

/// Compose predicates with `||`
let inline (|?|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x || pred2 x

type UserState  = unit 
type Parser<'t> = Parser<'t,UserState>
/// toml approved whitespace is ' ' or '\t'
let toml_space: Parser<_> = satisfy ((=)' '|?|(=)'\t')
let tspc = toml_space

let toml_spaces: Parser<_> = manySatisfy ((=)' '|?|(=)'\t')
let skip_toml_spaces : Parser<_> = skipManySatisfy ((=)' '|?|(=)'\t')

let tspcs = toml_spaces
let skip_tspcs = skip_toml_spaces

(*  Punctuation Parsers *)

let ``.``: Parser<_>    = pchar '.'
let ``,``: Parser<_>    = attempt (skip_tspcs >>. pchar ',' .>> skip_tspcs)  
let ``#``: Parser<_>    = pchar '#'
let ``[``: Parser<_>    = skip_tspcs >>. pchar '['    .>> skip_tspcs           
let ``]``: Parser<_>    = skip_tspcs >>. pchar ']'    .>> skip_tspcs
let ``{``: Parser<_>    = skip_tspcs >>. pchar '{'    .>> skip_tspcs
let ``}``: Parser<_>    = skip_tspcs >>. pchar '}'    .>> skip_tspcs 
let ``[[``: Parser<_>   = skip_tspcs >>. pstring "[[" .>> skip_tspcs  
let ``]]``: Parser<_>   = skip_tspcs >>. pstring "]]" .>> skip_tspcs  
let ``"``: Parser<_>    = pchar '"'
let ``'``: Parser<_>    = pchar '\''
let ``"""``: Parser<_>  = pstring "\"\"\""
let ``'''``: Parser<_>  = pstring "\'\'\'"
let skipEqs : Parser<_> = skip_tspcs >>. skipChar '=' >>. skip_tspcs

(*  Comment/LineEnd Parsers *)


let pComment = ``#``.>>. restOfLine false
let skipComment : Parser<_> = skipChar '#' >>. skipRestOfLine  true
let tskipRestOfLine = skipComment <|>  skipRestOfLine  true


(*  String Parsers  *)

let psingle_string : Parser<_> = 
    between ``"`` ``"`` (manySatisfy ((<>)'"'))

let pmult_string : Parser<_> = 
    between ``"""`` ``"""`` (manyChars anyChar)

let psingle_litstring : Parser<_> = 
    between ``'`` ``'`` (manySatisfy ((<>)'\''))

let pmult_litstring : Parser<_> = 
    between ``'''`` ``'''`` (manyChars anyChar)

let pString_toml : Parser<_> = psingle_string <|> pmult_string


(*|-----------------|*)
(*| Numeric Parsers |*)
(*|-----------------|*)

let pint64_toml : Parser<_> = 
    followedByL (satisfy ((<>)'0')) "TOML ints cannot begin with leading 0s"
    >>. many1Chars (skipChar '_' >>. digit <|> digit)
    .>> notFollowedByL ``.`` "TOML ints cannot contain `.`"
    |>> int64


// TODO - Proper checks for `_` rules and `.` count
//[<MethodImpl (MethodImplOptions.AggressiveInlining)>]

let pfloat_toml : Parser<_> = 
    let floatChar = satisfy (isDigit|?|isAnyOf['e';'E';'+';'-';'.'])
    followedByL (satisfy ((<>)'0')) "TOML floats cannot begin with leading 0s"  
    >>. many1Chars (skipChar '_' >>. floatChar <|> floatChar)
    |>> float


let private toDateTime str =
    let mutable dt = Unchecked.defaultof<DateTime>
    match DateTime.TryParse (str, &dt) with
    | false -> failwithf "failed paring into DateTime - %s" str
    | true  -> dt


let pDateTime_toml : Parser<_> =
    manySatisfy (isDigit|?|isAnyOf['T';':';'.';'-';'Z']) |>> toDateTime


(*  Simple Value Parsers *)

let pBool_toml : Parser<_> = 
    (pstring "false" >>% false) <|> (pstring "true" >>% true)

let private toml_simval : Parser<_> =
    choice [
        attempt pint64_toml  |>> Value.Int
        pfloat_toml          |>> Value.Float
        pString_toml         |>> Value.String
        pBool_toml           |>> Value.Bool
        pDateTime_toml       |>> Value.DateTime
    ]


(*  Key Parsers *)

let pBareKey : Parser<_> = 
    many1Satisfy (isDigit|?|isLetter|?|isAnyOf['_';'-']) 


let pQuoteKey : Parser<_> = 
    between ``"`` ``"`` (many1Chars anyChar) 


let toml_key : Parser<_> =
    choice [pBareKey |>> Key.Bare; pQuoteKey |>> Key.Quoted]


let pTableKey : Parser<_> = 
    between ``[`` ``]`` (sepBy pBareKey ``.``)

let pTableArrayKey : Parser<_> = 
    between ``[[`` ``]]`` (sepBy pBareKey ``.``)



(*  Collection Parsers *)


// Forward declaration to allow mutually recursive 
// parsers between arrays and inline tables
let private pArr,  private pArrImpl  = createParserForwardedToRef ()
let private pITbl, private pITblImpl = createParserForwardedToRef ()

let toml_array : Parser<_> =
    pArrImpl := 
        between  ``[`` ``]``
            (sepBy (pArr <|> pITbl <|> toml_simval) ``,``)
        |>> Value.Array
    pArr 


let toml_inlineTable : Parser<_> =
    let pitem = toml_key .>>. (skipEqs >>. toml_simval)
    let pArr  = toml_key .>>. (skipEqs >>. toml_array)
    pITblImpl :=
        between ``{`` ``}``
            (sepBy (tspcs >>. (pitem <|> pArr)) ``,``)
        |>> fun items ->
            let tbl:(_,_) table = table<_,_> ()
            List.iter tbl.Add items
            Value.InlineTable tbl
    pITbl


(*  Toplevel Parsers  *)


let toml_value : Parser<_> = 
    choice [toml_simval; toml_array; toml_inlineTable]


let toml_item : Parser<item> =
    toml_key .>>. (skipEqs >>. toml_value)




(*
    TODO - build low level choice parser for TOML?

    Top Level
    ---------
        - Table Array   : starts with `[[`
        - Table         : starts with a `[`
        - Quoted Key    : starts with '"'
        - Bare Key      : ^ not those, not whitespace
            

    After a key
    -----------
        - DateTime      : check based on `-` @pos 5 in 1980-23-...
        - Array         : starts with `[`
        - Inline Table  : starts with a `{`
        - Integer       : starts with +|-|digit
        - Float         : ^ + contains e|E|.|+|-
        - Boolean       : starts with t|f 
        - String        : starts with '"'


        Could do if digit then (float <|> int <|> datetime)

    Example of Low Level Parser
    ---------------------------

    If you look at the definitions for the argument parsers, you’ll see that in almost all cases one can
    decide which parser should handle the input just based on the next char in the input. Hence, we could 
    replace the choice‐based parser with the following low‐level implementation:

    let error = expected "JSON value"
    fun (stream: CharStream<_>) ->
        match stream.Peek() with
        | '{' -> jobject stream
        | '[' -> jlist stream
        | '"' -> jstring stream
        | 't' when stream.Skip("true")  -> Reply(JBool true)
        | 'f' when stream.Skip("false") -> Reply(JBool false)
        | 'n' when stream.Skip("null")  -> Reply(JNull)
        | _ ->
            let stateTag = stream.StateTag
            let mutable reply = jnumber stream
            if reply.Status = Error && stateTag = stream.StateTag then
               reply.Error <- error
            reply
        
*)


