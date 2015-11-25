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
    (ls, init) ||> List.foldBack (fun elm acc -> mergeErrors acc elm.Error)

let internal mergeResult (rs:Reply<_> list) =
    let output = (rs,[]) ||> List.foldBack (fun elm acc  -> elm.Result::acc)
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
//    |> fun x -> 
//        printfn "\nSection Splitter\n"
//        x.Result |> List.iter (fun (k,v) -> printfn "%s -- %s" k v )
//        x

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
//    |> fun x -> 
//        printfn "\n>> Section Parser\n"
//        x.Result |> List.iter (fun (k,v) -> printfn "%s -- %s" k (string v) )
//        x

pSectionImpl := section_parser .>> skip_tspcs


let inline makeTable (vals: #seq<string*Value>): (_,_)table =
    let tbl = table<_,_>() 
    tbl.AddSeq vals 
    tbl

//let keyName (key:string) = 
//    let trimd = 
//        if String.bookends "[" "]" key then key.Substring(1,key.Length-1)
//        else key
//    (trimd.Split '.').Last 

let parse_block (keybracket:string,sectiondata:string) =
    let isAoT = String.bookends "[[" "]]" keybracket
    let tkey  = if isAoT then keybracket.[2..keybracket.Length-3] else 
                keybracket.[1..keybracket.Length-2]
    let psr_result = 
        sectiondata |> run (toml_section
            |>> List.map (fun (k,v) -> 
                    String.concat""[tkey;".";k],v )) 
    match psr_result with
    | Failure (errorMsg,_,_) -> failwith errorMsg
    | Success (result,_,_)   -> 
//        printfn "\n[- Parse Block -]\n"
//        result |> List.iter (fun (k,v) -> printfn "%s -- %s" k (string v) )
        keybracket, result

    



let inline keyName (key:string) = (key.Split '.').Last 


let inline parentKey (key:string)  = 
    printfn "initial parentkey - %s" key
    let tkey =
        if   String.bookends "[[" "]]" key then key.[2..key.Length-3]
        elif String.bookends "["   "]" key then key.[1..key.Length-2]
        else key
    let idx1 = tkey.LastIndexOf '.' 
    let pkey =
        if idx1 = -1 then String.Empty else
        let sub = tkey.Substring(0,idx1) in let idx2 = sub.LastIndexOf '.' 
        if idx2 = -1 then sub else sub.Substring(idx2+1, sub.Length-idx2-1)
    printfn "cleaned parentkey - %s" pkey
    pkey


let getNested = parentKey >> keyToList


let inline (|DisjointSections|IsRootOfPrev|AtTopLevel|) (prev, cur) =  
    let inline getRoot (key:string)  = 
        match key.IndexOf '.' with
        | -1 -> key | idx -> key.Substring(0,idx)
    if  getRoot prev = getRoot cur then IsRootOfPrev
    elif  parentKey prev = "" && parentKey cur = "" then AtTopLevel else
    DisjointSections


let inline (|AddToLastAoT|AddToCurrentAoT|NotAoT|) (prev, cur) =
    let isAoT header = String.bookends "[[" "]]" header
    if isAoT prev && isAoT cur && keyName cur = keyName prev then AddToLastAoT 
    elif isAoT cur then AddToCurrentAoT
    else NotAoT

let consOnAoT name curTbl (prevTable:(string,Value)table) = 
    let name = keyName name
    match prevTable.[name] with
    | Value.ArrayOfTables tbls ->  
        printfn "\ncons onto from of AoT - Add table into %s with data ::\n %s" (keyName name) (Value.Table curTbl|> string)

        prevTable.[name] <- Value.ArrayOfTables (curTbl::tbls)
    | v -> printfn "tried to add a table to %A, expected an array of tables\
                    should be a failure, but let's chill on that" v
    

// when we see that the previous section shares no roots with the current section
// it's time to connected the previous section up to the toplevel before we continue 
// constructing the datastructure
let addToToplevel prevName prevTable (toml:(string,Value)table) (nested:string list) =
    let rec loop nsd (name:string,tbl:(string,Value)table) =
        match nsd with
        | [] -> 
            printfn "\nadd to toplevel %s = %s" (keyName name) (Value.Table tbl|> string)
            toml.Add(keyName name,Value.Table tbl); toml

        | hd::_ -> 
            let newtbl = makeTable[] in newtbl.Add(keyName name,Value.Table tbl)
            loop [] (hd,newtbl)
    loop nested  (prevName,prevTable)


// current name has been split to remove qualification
let connectToRoot (curName:string)   (curRoot:(string,Value)table) 
             (prevName:string) (prevTable:(string,Value)table) (prevNest:string list) =

    let rec checkNested (ls:string list) acc =
        match ls with
        | hd::tl -> 
            if hd.Contains "." then let fls = keyToList hd |> List.rev in checkNested tl (fls@acc)
            else checkNested tl (hd::acc)
        | [] -> acc

    let rec loop nsd prevName (acc:(string,Value)table) =
        match nsd with
        | hd::_ when hd = curName -> 
            printfn "\nIn connecting to root - %s = %s" (keyName prevName) (Value.Table acc|> string)

            curRoot.Add(keyName prevName,Value.Table acc); curRoot
        | _::tl ->  
            let newTable = makeTable[] in newTable.Add(keyName prevName,Value.Table acc)
            printfn "\nIn Connecting to root %s = %s" (keyName prevName) (Value.Table acc|> string)
            loop tl prevName newTable
        | _ -> failwithf "improper nesting list provided %A\n List should have contained - %s" prevNest curName
    loop (checkNested prevNest []  ) prevName prevTable




(* Need a function to handle recursive ascent construction *)






let construct_toml (toplevel:(string*Value)list, 
                    tables:(string*(string*Value)list)[]) =

    let addSection (table:Dictionary<string,Value>) (kvps:(string*Value) list) =
        kvps |> List.iter (fun (k,v)-> table.Add( keyName k,v ))

    let foldtoml    (prev:string, nested:string list, acc:Dictionary<string,Value>,toml:Dictionary<string,Value>) 
                    (cur:string, elems:(string*Value)list)       = 

        printfn    "CONSTRUCT TOML STATE ::\n\
                    prev    %s - \n\
                    nested  %s - \n\
                    cur     %s - \n\
                    elems   %A - \n\
                    acc     %A - \n\
                    toml    %A - \n" prev (string nested) cur elems (Value.Table acc |> string) (Value.Table toml |> string)


        (* add parsed elements to current table *)
        let curtbl = makeTable elems in addSection curtbl elems
        match  prev,cur with 
        | IsRootOfPrev & NotAoT ->
            let acc = connectToRoot cur curtbl prev acc nested
            printfn "\nIsRootOfPrev and NotAoT - connecting %s into -\n %s" 
                (keyName prev) (Value.Table curtbl|> string)
            (cur, getNested cur,acc,toml )

        | IsRootOfPrev & AddToCurrentAoT ->
            let curAoT = [connectToRoot cur curtbl prev acc nested]
            let parent = makeTable[] in parent.Add(keyName cur,Value.ArrayOfTables curAoT)
            printfn "\nIsRootOf Prev and AddToCurrentAoT - adding %s into AoT::\n %s" (keyName cur) (Value.ArrayOfTables curAoT|> string)
            let pkey = parentKey cur
            (pkey, getNested pkey,parent, toml)
        
        | DisjointSections & NotAoT ->
            let toml = addToToplevel prev acc toml nested
            printfn "\nDisjoint Sections and NotAoT- adding %s into ::\n %s" 
                (keyName prev) (Value.Table toml|> string)
            (cur, getNested cur, curtbl, toml)
            
        (* If we're creating a new AoT it's parent needs to be created to store it inside of *)
        | DisjointSections & AddToCurrentAoT  ->
            let toml = addToToplevel prev acc toml nested
            let parent = makeTable[] in parent.Add(keyName cur,Value.ArrayOfTables [curtbl])
            printfn "\nDisjoint Sections  and AddToCurrentAoT - adding %s into AoT::\n %s" 
                (keyName cur) (Value.Table parent|> string)
            let pkey = parentKey cur
            (pkey, getNested pkey,parent , toml)
        
        | AtTopLevel & NotAoT ->
            toml.Add(keyName prev, Value.Table acc)
            printfn "\nAtToplevel and NotAoT - adding %s into ::\n %s" 
                (keyName prev) (Value.Table acc|> string)
            (cur, getNested cur, curtbl, toml)

        | AtTopLevel &  AddToLastAoT ->    
            consOnAoT cur curtbl acc
            printfn "\nAtToplevel and AddToLastAoT- adding %s into ::\n %s" 
                (keyName prev) (Value.Table acc|> string)
            (cur, getNested cur,acc, toml)

        | AtTopLevel & AddToCurrentAoT ->
            let curAoT = [connectToRoot cur curtbl prev acc nested]
            printfn "\nAtTopLevel and AddToCurrentAoT - adding %s into AoT::\n %s" 
                (keyName cur) (Value.ArrayOfTables curAoT|> string)

            toml.Add(keyName cur,Value.ArrayOfTables curAoT )
            (cur, getNested cur,toml, toml)

        | AddToLastAoT ->    
            consOnAoT cur curtbl acc
            printfn "\nIsRootOf Prev and AddToCurrentAoT - adding %s into AoT::\n %s" 
                        (keyName cur) (Value.Table curtbl|> string)
            (cur, getNested cur,acc, toml)
        (* If all else fails, keep on trucking??? *)
        | _, _ -> 
            printfn "kept on trucking\n cur is - %s" cur
            ( cur, nested, acc, toml)

    let tables = tables |> Array.filter (fun (_,ls)-> ls<>[])
    let initToml, acc = makeTable[], makeTable[]
    let _,_,_,toml = Array.fold foldtoml ("Start-Folding",[],acc,initToml) tables
    addSection toml toplevel

//
//    let toml = TOML()
//    List.iter toml.Add toplevel
//    tables |> Array.iter (fun (tkey,kvps)  -> 
//        let isAoT = String.bookends "[[" "]]" tkey
//        let name  = if isAoT then tkey.[2..tkey.Length-3] else 
//                    tkey.[1..tkey.Length-2]
//
//        let trimd = kvps |> Seq.map (fun (k,v) -> keyName k,v )
//        let tbl = table<_,_>() in tbl.AddSeq(trimd)
//        if isAoT then
//            toml.Add (name,Value.Table tbl,true)
//        else
//            toml.Add(name,Value.Table tbl)
//        )
    printfn "Toml as Value.Table\n\n%s" (string (Value.Table toml) )
    printfn "Toml as Dictionary \n\n%A" toml
    toml

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

    tables 
    |> Array.filter (fun (_,ls)-> ls<>[])
    |> Array.map( fun x -> printfn "%A" x; x )
    |> Array.iter (fun (name,kvpls) -> 
        sb.AppendLine().AppendLine(name).AppendLine()|>ignore
        match kvpls with [] -> () | _  -> format_section kvpls)

    sb.AppendLine().AppendLine("-- toplevel --").AppendLine()|>ignore

    format_section toplevel


    string sb

let tomstructor =
    toml_section .>>. (section_splitter |>> fun ls ->
        Array.ofList ls |> Array.Parallel.map parse_block)
    |>> construct_toml

// NOTE - not handling arrays of tables properly
let parse_to_print = 
    toml_section .>>. (section_splitter |>> fun ls ->
        Array.ofList ls |> Array.Parallel.map parse_block)
    |>> sprint_parse_array 
    
