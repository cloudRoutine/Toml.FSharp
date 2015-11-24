module TomlFs.Parsers
#nowarn "62"
open System
open System.Collections.Generic
open FParsec
open FParsec.Primitives
open TomlFs.AST
open System.Text

type UserState = unit    
type Parser<'t> = Parser<'t,UserState>


(*| Whitespace/Comment/LineEnd AKA Skip Parsers |*)

/// toml approved whitespace is ' ' or '\t'
let skip_tspcs      : Parser<_>  = skipManySatisfy (isAnyOf [' ';'\t'])
let tskipRestOfLine : Parser<_>  = choice [ skipChar '#' >>. skipRestOfLine  true  
                                            skipRestOfLine  true ]


(*| Punctuation Parsers |*)

let ``.``   : Parser<_> = pchar   '.'  .>> skip_tspcs 
let ``,``   : Parser<_> = pchar   ','  .>> skip_tspcs   
let ``[``   : Parser<_> = pchar   '['  .>> skip_tspcs              
let ``]``   : Parser<_> = pchar   ']'  .>> skip_tspcs   
let ``{``   : Parser<_> = pchar   '{'  .>> skip_tspcs   
let ``}``   : Parser<_> = pchar   '}'  .>> skip_tspcs    
let ``[[``  : Parser<_> = pstring "[[" .>> skip_tspcs  
let ``]]``  : Parser<_> = pstring "]]" .>> skip_tspcs  
let ``"``   : Parser<_> = pchar '"'
let ``'``   : Parser<_> = pchar '\''
let ``"""`` : Parser<_> = pstring "\"\"\""
let ``'''`` : Parser<_> = pstring "\'\'\'"

let inline isEscChar c = c = '\\'
// parsers for string bounds that won't be fooled by escaped quotes
let prevCharNot = previousCharSatisfiesNot
let prevCharIs  = previousCharSatisfies 
let ``|"|``   : Parser<_> = prevCharNot isEscChar >>. pchar '"'
let ``|'|``   : Parser<_> = prevCharNot isEscChar >>. pchar '\''
let ``|"""|`` : Parser<_> = prevCharNot isEscChar >>. pstring "\"\"\""
let ``|'''|`` : Parser<_> = prevCharNot isEscChar >>. pstring "\'\'\'"
let skipEqs   : Parser<_> = skipChar '=' >>. skip_tspcs

let combo = [ 0x00..0x1f ] |> List.reduce (fun acc elm -> acc ||| elm)

// unicode control chars
let ctrlChar  : Parser<_> = 
    prevCharIs isEscChar 
    >>. satisfyL (isAnyOf [for hex in 0x00..0x1f -> char hex]) 
        "Unicode control charaters must be escaped"


(*| String Parsers |*)

// TODO - fully implement string spec
// probably need to make a low level string parser

let psingle_string    : Parser<_> = between ``|"|``   ``|"|``   (manyChars (choice
                                                                   [prevCharIs isEscChar >>. ``"``
                                                                    satisfy (isNoneOf['"'])]))
let pmult_string      : Parser<_> = between ``|"""|`` ``|"""|`` (manyChars anyChar)
let psingle_litstring : Parser<_> = between ``|'|``   ``|'|``   (manySatisfy (isNoneOf['\'']))
let pmult_litstring   : Parser<_> = between ``|'''|`` ``|'''|`` (manyChars anyChar)
let pString_toml      : Parser<_> =
    choice [psingle_string; pmult_string; psingle_litstring; pmult_litstring] .>> skip_tspcs


(*| Simple Value Parsers |*)

let pInt64_toml : Parser<_> = 
    followedByL (satisfy ((<>)'0')) "TOML ints cannot begin with leading 0s"
    >>. many1Chars (prevCharIs isDigit >>. skipChar '_' >>. digit <|> digit)
    .>> notFollowedByL ``.`` "TOML ints cannot contain `.`" |>> int64

let isFloatChar = isDigit|?|isAnyOf['e';'E';'+';'-';'.']

let pFloat_toml : Parser<_> = 
    let floatChar = satisfy isFloatChar
    followedByL (satisfy ((<>)'0')) "TOML floats cannot begin with leading 0s"  
    >>. many1Chars (skipChar '_' >>. floatChar <|> floatChar) |>> float

let private toDateTime str =
    let mutable dt = Unchecked.defaultof<DateTime>
    match DateTime.TryParse (str, &dt) with
    | false -> failwithf "failed parsing into DateTime - %s" str
    | true  -> dt

let isDateChar = isDigit|?|isAnyOf['T';':';'.';'-';'Z']

let pDateTime_toml : Parser<_> = manySatisfy isDateChar |>> toDateTime
let pBool_toml     : Parser<_> = (pstring "false" >>% false) <|> (pstring "true" >>% true)

let toml_int      = pInt64_toml     |>> Value.Int
let toml_float    = pFloat_toml     |>> Value.Float
let toml_datetime = pDateTime_toml  |>> Value.DateTime
let toml_bool     = pBool_toml      |>> Value.Bool
let toml_string   = pString_toml    |>> Value.String


(*| Key Parsers |*)

// key formats

let isKeyStart = isDigit|?|isLetter|?|isAnyOf['"';'\'']

let pBareKey          : Parser<_> = many1Satisfy (isDigit|?|isLetter|?|isAnyOf['_';'-']) 
let pQuoteKey         : Parser<_> = between ``"`` ``"`` (many1Chars anyChar) 
// key in a collection
let toml_key          : Parser<_> = 
    (choiceL [pBareKey; pQuoteKey ] 
        "a quoted key starting with \"\
         or a bare key starting with a letter or digit\n") .>> skip_tspcs
// toplevel keys
let pTableKey         : Parser<_> = between ``[``   ``]`` (sepBy toml_key ``.``)
let pArrayOfTablesKey : Parser<_> = between ``[[`` ``]]`` (sepBy toml_key ``.``)


(*  Collection Parsers *)

// Forward declaration to allow mutually recursive 
// parsers between arrays and inline tables
let toml_value      , private pValueImpl  = createParserForwardedToRef ()
let toml_array      , private pArrayImpl  = createParserForwardedToRef ()
let toml_inlineTable, private pITblImpl   = createParserForwardedToRef ()

let pKVP : Parser<_>  = skip_tspcs >>. toml_key .>>. (skipEqs >>. toml_value)

let pArray_toml : Parser<_> = 
    let ``[``   : Parser<_> = (attempt (``[`` .>> unicodeNewline .>> skip_tspcs)) <|> ``[`` 
    let ``]``   : Parser<_> = (attempt (skip_tspcs .>> unicodeNewline .>> skip_tspcs >>. ``]``)) <|> ``]`` 
    let ``,``   : Parser<_> = (attempt (``,`` .>> unicodeNewline .>> skip_tspcs)) <|> ``,`` 
    between ``[`` ``]`` (sepBy toml_value ``,``) 

let pInlineTable : Parser<_> =
    between ``{`` ``}`` (sepBy pKVP ``,``)
    |>> fun items ->
        let tbl:(_,_) table = table<_,_> ()
        List.iter tbl.Add items
        tbl

pArrayImpl := pArray_toml  |>> Value.Array       .>> skip_tspcs
pITblImpl  := pInlineTable |>> Value.InlineTable .>> skip_tspcs

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
        let state, reply = stream.State, toml_int stream
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

let inline listToKey keys = String.concat "." keys
let inline keyToList (key:string) = key.Split '.' |> Array.toList

// Brace matching parsers that store the chars unlike preceding versions
let ``[+``  = pstring "["  .>> skip_tspcs 
let ``]+``  = pstring "]"  .>> skip_tspcs
let ``[[+`` = pstring "[[" .>> skip_tspcs 
let ``]]+`` = pstring "]]" .>> skip_tspcs

let ptkey : Parser<_> =  
    ``[+``.>>. (sepBy toml_key ``.``) .>>. ``]+``
    |>> fun ((b,ls),e) -> String.concat "" [b; listToKey ls; e]

let pakey : Parser<_> = 
    ``[[+``.>>. (sepBy toml_key ``.``) .>>. ``]]+``
    |>> fun ((b,ls),e) -> String.concat "" [b; listToKey ls; e]

let raw_content_block =
    manyCharsTill anyChar (followedByL (pakey<|>ptkey) "expected a table or array table key"<|>eof)

let toml_item = (toml_key .>>. (skipEqs >>. toml_value)) .>> skip_tspcs

let internal foldErrors init (ls:Reply<_> list) =
    (init, ls) ||> List.fold (fun acc elm -> mergeErrors acc elm.Error)

let internal mergeResult (rs:Reply<_> list) =
    let output = ([],rs) ||> List.fold (fun acc elm -> elm.Result::acc)
    Reply (Ok,output,NoErrorMessages)


// split the TOML file up into sections by their table keys
let section_splitter (stream: CharStream<_>) =
    
    let rec loop acc stream =

        let inline checkReply (psr:Parser<_>) acc =
            let (reply: _ Reply) =  psr stream
            if reply.Status <> Ok then Reply (Error, reply.Error) 
            else loop (reply::acc) stream

        match stream.Peek () with
        | '['  ->   
            if stream.Peek2 () = TwoChars ('[','[') 
            then checkReply (pakey.>>.raw_content_block) acc 
            else checkReply (ptkey.>>.raw_content_block) acc  
        | '#' -> 
            stream.SkipRestOfLine true 
            loop acc stream
        | ' ' | '\t' -> 
            stream.SkipUnicodeWhitespace() |> ignore 
            loop acc stream 
        | '\n' -> 
            if not (stream.SkipUnicodeNewline ()) 
            then mergeResult acc 
            else loop acc stream
        | c when isKeyStart c -> stream.SkipRestOfLine true; loop acc stream
        | c ->   
            if stream.IsEndOfStream 
            then mergeResult acc else
            let lastError = (expected <| sprintf "could not parse unexpected character -'%c'\
                                                  at Ln: %d Col: %d" c stream.Line stream.Column)
            Reply (Error, foldErrors lastError acc)
    loop [] stream

let toml_section, private pSectionImpl  = createParserForwardedToRef ()

let private section_parser (stream:CharStream<_>) =
    let rec loop acc (stream:CharStream<_>) =
        match stream.Peek () with
        | '#' ->  stream.SkipRestOfLine true;  loop acc stream
        | ' ' | '\t' -> stream.SkipUnicodeWhitespace () |> ignore; loop acc stream 
        | '\n' ->       stream.SkipUnicodeNewline () |> ignore; loop acc stream
        | '['  -> mergeResult acc  
        | c when isKeyStart c -> loop (toml_item stream::acc) stream
        | c -> 
            if stream.IsEndOfStream then mergeResult acc else
            let lastError = (expected <| sprintf "could not parse unexpected character -'%c'\
                                                  at Ln: %d Col: %d" c stream.Line stream.Column)
            let result = (mergeResult acc).Result
            Reply(Error,result,lastError)
    loop [] stream


pSectionImpl := section_parser .>> skip_tspcs


let parse_block (keybracket:string,sectiondata:string) =
    let isAot = String.bookends "[[" "]]" keybracket
    let tkey  = if isAot then keybracket.[2..keybracket.Length-3] else 
                keybracket.[1..keybracket.Length-2]
    let psr_result = 
        sectiondata |> run (toml_section
            |>> fun xs -> xs |> List.map (fun (k,v) -> 
                String.concat""[tkey;".";k],v )) 
    match psr_result with
    | Failure (errorMsg,_,_) -> failwith errorMsg
    | Success (result,_,_)   -> keybracket, result
    
let inline makeTable (vals: #seq<string*Value>): (_,_)table =
    let tbl = table<_,_>() 
    tbl.AddMany vals 
    tbl



let internal sprint_parse_array (toplevel,tables) =
    let sb = StringBuilder()

    let findmaxes (kvpls:(string*Value)list) =
        ((0,0),kvpls) 
        ||> Seq.fold (fun (kmax,vmax) (key,value) ->
            max kmax key.Length, max vmax (string value).Length)

    let format_section ls =
        let maxklen, maxvlen = findmaxes ls
        ls |> List.iter(fun (key,value) ->
            sprintf "%-*s = %-*s" maxklen key maxvlen (string value)
            |> sb.AppendLine |> ignore )

    sb.AppendLine().AppendLine("-- toplevel --").AppendLine()|>ignore

    format_section toplevel

    tables 
    |> Array.iter (fun (name,kvpls) -> 
        sb.AppendLine().AppendLine(name).AppendLine()|>ignore
        match kvpls with [] -> () | _  -> format_section kvpls)

    string sb


// NOTE - not handling arrays of tables properly
let parse_to_print = 
    toml_section .>>. (section_splitter |>> fun ls ->
        Array.ofList ls |> Array.Parallel.map parse_block)
    |>> sprint_parse_array 
    
