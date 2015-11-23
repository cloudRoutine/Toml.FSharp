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
//type Parser<'t> = Parser<'t,TomlState>
type Parser<'t> = Parser<'t,UserState>
//type Parser<'t> = Parser<'t,String>


(*| Whitespace Parsers |*)

/// toml approved whitespace is ' ' or '\t'
let ws = isAnyOf [' ';'\t']
let toml_space       : Parser<_> = satisfy ws
let toml_spaces      : Parser<_> = manySatisfy ws
let skip_toml_spaces : Parser<_> = skipManySatisfy ws

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

// let [<Literal>] unicodeCtrlChar = combo
let [<Literal>] unicodeCtrlChar = 0x00 ||| 0x01 ||| 0x1f // ||| ... 


// unicode control chars
let ctrlChar  : Parser<_> = 
    prevCharIs isEscChar 
    >>. satisfyL (isAnyOf [for hex in 0x00..0x1f -> char hex]) 
        "Unicode control charaters must be escaped"

(*| String Parsers |*)

// TODO - fully implement string spec
// probably need to make a low level string parser
let psingle_string    : Parser<_> = 
    between ``|"|`` ``|"|`` 
        (manyChars ((prevCharIs isEscChar >>. ``"``) <|> satisfy ((<>)'"'))) 

let pmult_string      : Parser<_> = between ``|"""|`` ``|"""|`` (manyChars anyChar)
let psingle_litstring : Parser<_> = between ``|'|``   ``|'|``   (manySatisfy (isNoneOf['\'']))
let pmult_litstring   : Parser<_> = between ``|'''|`` ``|'''|`` (manyChars anyChar)

let toml_string : Parser<_> =
    (choice [psingle_string; pmult_string; psingle_litstring; pmult_litstring]
        |>> Value.String) .>> skip_tspcs


(*| Numeric Parsers |*)

let pInt64_toml : Parser<_> = 
    followedByL (satisfy ((<>)'0')) "TOML ints cannot begin with leading 0s"
    >>. many1Chars (prevCharIs isDigit >>. skipChar '_' >>. digit <|> digit)
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
let pArrayOfTablesKey : Parser<_> = between ``[[`` ``]]`` (sepBy toml_key ``.``)


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

let ``[+``  = pstring "["  .>> skip_tspcs 
let ``]+``  = pstring "]"  .>> skip_tspcs
let ``[[+`` = pstring "[[" .>> skip_tspcs 
let ``]]+`` = pstring "]]" .>> skip_tspcs

let ptkey : Parser<_> =  
    ``[+``.>>. (sepBy toml_key ``.``) .>>. ``]+``
    |>> fun ((b,ls),e) -> String.concat "" [b;listToKey ls;e]

let pakey : Parser<_> = 
    ``[[+``.>>. (sepBy toml_key ``.``) .>>. ``]]+``
    |>> fun ((b,ls),e) -> String.concat "" [b;listToKey ls;e]

let content_block =
    manyCharsTill anyChar (followedByL(pakey<|>ptkey) "expected a table or array table key"<|>eof)

let toml_item = 
    ((toml_key .>>. (skipEqs >>. toml_value)) .>> tskipRestOfLine)

let toml_start : Parser<_> =
    skipManyTill anyChar (followedByL toml_item "expected a toml item")
    >>. (many (skip_tspcs>>. toml_item))

let foldErrors init (ls:Reply<_> list) =
    (init, ls) ||> List.fold (fun acc elm -> mergeErrors acc elm.Error)

let mergeResult (rs:Reply<_> list) =
    let output = ([],rs) ||> List.fold (fun acc elm -> elm.Result::acc)
    Reply (Ok,output,NoErrorMessages)

// split the TOML file up into sections by their table keys
let section_parser (stream: CharStream<_>) =
    
    let rec loop acc stream =

        let inline checkReply (psr:Parser<_>) acc =
            let (reply: _ Reply) =  psr stream
            if reply.Status <> Ok then Reply (Error, reply.Error) 
            else loop (reply::acc) stream

        match stream.Peek () with
        | '['  ->   
            if stream.Peek2 () = TwoChars ('[','[') 
            then checkReply (pakey.>>.content_block) acc 
            else checkReply (ptkey.>>.content_block) acc  
        |'#' -> 
            stream.SkipRestOfLine true 
            loop acc stream
        |' '|'\t' -> 
            stream.SkipUnicodeWhitespace() |> ignore 
            loop acc stream 
      
        | '\n' -> 
            if not (stream.SkipUnicodeNewline ()) 
            then mergeResult acc 
            else loop acc stream
        | c when (isDigit|?|isLetter|?|isAnyOf['"';'\'';']']) c -> 
            stream.SkipRestOfLine true 
            loop acc stream
        | c->   
            if stream.IsEndOfStream 
            then mergeResult acc else
            let lastError = (expected <| sprintf "could not parse unexpected character -'%c'\
                                                  at Ln: %d Col: %d" c stream.Line stream.Column)
            Reply (Error, foldErrors lastError acc)
    loop [] stream


let parseIntoTableArray (parseKey:string list, inlineTables) =
    // parsekey of a tablearray gives us the table that stores this inlinetable
    // and the key for this inlinetable
    let intbl =
        (table<_,_>(), inlineTables) 
        ||> List.fold (fun tbl (key,value) -> tbl.Add(key,value); tbl) 
    parseKey, Value.InlineTable intbl


//let toplevel = choice [toml_item;toml_table;toml_tableArray]

//pTableImpl := (pTableKey .>> tskipRestOfLine) .>>. (many toml_item)
//    |>> fun (ks,items) -> 
//        let tk = List.last ks
//        let tbl:(_,_) table = table<_,_> ()
//        List.iter (tbl.Add) items
//        tk, tbl )


//pTableArrayImpl := ((pArrayOfTablesKey .>> tskipRestOfLine) .>>. many toml_item)
//    |>> fun ((aotkey:string list),items) -> 
//        let ak = List.last ks
//        let arr =
//            tbls |> List.map (fun (key,value) -> 
//            let tbl:(_,_) table = table<_,_> ()
//            tbl.Add(key,value)
//            tbl) 
//        ak, Value.TableArray arr

    
let inline private print str x = printfn "%s%A" str x; x


let toml_table, private pTableImpl = createParserForwardedToRef ()
let toml_aot, private pArrOfTablesImpl = createParserForwardedToRef ()



let makeTable (vals: #seq<string*Value>): (_,_)table =
    let tbl = table<_,_>() 
    tbl.AddMany vals 
    tbl

let toml_tablekey = (pTableKey .>> tskipRestOfLine) 





pTableImpl := 
    (pTableKey .>> tskipRestOfLine).>>. (many toml_item)

pArrOfTablesImpl :=
    many ((pArrayOfTablesKey .>> tskipRestOfLine) .>>. many toml_item)

(*
    Rewrite parser to split on 

        [table.names]
        [[and.arrayof.table.names]]

        #   pull the heading of the table/array into the
        #   the components on the inside for fully qualified names

        e.g
            title                   = TOML Example
            owner.name              = Tom Preston-Werner
            owner.organization      = GitHub
            owner.bio               = GitHub Cofounder & CEO\nLikes tater tots and beer.
            owner.dob               = 05/27/1979 03:32:00
            database.server         = 192.168.1.1
            database.ports          = [| 8001, 8001, 8002, |]
            database.connection_max = 5000
            database.enabled        = True

    Build up a hashset during the parse to prevent any duplicate keys
*)



let toml_parser (stream: CharStream<_>) =
    let toml = TOML (table<_,_> ())

    let rec loop acc (stream: CharStream<_>) : _ Reply =

        let inline checkReply (psr:Parser<_>) =
            let state, (reply: _ Reply)  = stream.State, psr stream
            if reply.Status <> Ok then stream.BacktrackTo state; Reply (Error, reply.Error) else 
            loop reply stream

        let tablels (tbl:table<string,Value>) = 
            tbl |> Seq.map(fun kvp -> (kvp.Key,kvp.Value)) |>List.ofSeq

        let rec addItems keybase ls =
            match ls with 
            | [] -> toml
            | (k,v)::tl ->
                let key = String.concat "" [keybase;".";k]
                match v with
                | Value.Table tbl -> addItems key (tablels tbl)
                | Value.ArrayOfTables ars ->
                    let arr = (List.map Value.InlineTable ars)
                    if toml.ContainsKey key then 
                        match toml.[key] with
                        | Value.Array ls ->
                            toml.[key] <- Value.Array(ls@arr); addItems keybase tl
                        | _ ->  addItems keybase tl
                    else
                        toml.Add (key,Value.Array arr); addItems key tl
                | v ->  toml.Add (key,v); addItems keybase tl

        match stream.Peek () with
        | '#'  -> stream.SkipRestOfLine true; loop acc stream
        | ' ' | '\t' -> if not (stream.SkipUnicodeWhitespace ()) then acc else loop acc stream 
        | '['  -> 
            if stream.Peek2 () = TwoChars ('[','[') then 
                checkReply (toml_aot|>> fun arrs ->
                    arrs |> List.iter (fun (tkey,items) -> addItems (listToKey tkey) items |> ignore) 
                    toml)
            else 
                checkReply (toml_table |>> fun (tkey,items) -> addItems (listToKey tkey) items)
        | c when (isDigit|?|isLetter|?|isAnyOf['"';'\'']) c ->
            checkReply (toml_item|>> fun (key,value) ->
                toml.Add (key,value)
                toml)
        | '\n' -> if not (stream.SkipUnicodeNewline ()) then acc else loop acc stream
        | c   -> 
            if stream.IsEndOfStream then acc else
                let lastError =
                    (expected <| sprintf "could not parse unexpected character -'%c'\
                                            at Ln: %d Col: %d" c stream.Line stream.Column)
                let errorls = mergeErrors acc.Error lastError
                Reply (Error, errorls)
    loop (Reply<_>()) stream



let parse_toml = fun stream -> toml_parser stream

(* 
    ---- NOTES ----
    
    TODO - Proper checks for `_` rules and `.` count
    
    Does [<MethodImpl (MethodImplOptions.AggressiveInlining)>] do anything worthwhile?

*)
