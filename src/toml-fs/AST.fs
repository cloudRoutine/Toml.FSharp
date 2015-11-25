module TomlFs.AST
#nowarn "62"
open System
open System.Text
open System.Collections.Generic

type ('key,'value) Dictionary with
    member self.TryAdd (key,value) =
        if self.ContainsKey key then false else self.Add (key,value); true

    member self.AddSeq xs = Seq.iter self.Add xs

    member self.AddOnlyNewKeys ls = ls |> List.iter (self.TryAdd>>ignore)


let inline private seqstr xs (modstr:string) =
    let sb = System.Text.StringBuilder()
    xs |> Seq.iter (fun x -> sb.Append(string x).Append(",").Append(modstr)|>ignore)
    string sb  

type ('k,'v) table = ('k,'v) Dictionary

type [<RequireQualifiedAccess>] 
    Key = 
    | Bare of string | Quoted of string
    override key.ToString () = key |> function Bare k -> k | Quoted k -> k 

and [<RequireQualifiedAccess>] 
    [<StructuredFormatDisplay "{Display}">]
    Value =
    | String        of string
    | Int           of int64
    | Float         of float
    | Bool          of bool
    | DateTime      of DateTime
    | InlineTable   of (string, Value) table   
    | Table         of (string, Value) table   
    | Array         of Value list
    | ArrayOfTables of ((string, Value) table) list
    override value.ToString () =
        let rec loop (indent:int) (toml:Value) =
            match toml with
            | String v       -> v
            | Int v          -> string v
            | Float v        -> string v
            | Bool v         -> string v
            | DateTime v     -> string v
            | InlineTable vs -> 
                ((StringBuilder(),vs) ||> Seq.fold (fun sb elm -> 
                    sb.Append(elm.Key).Append(" = ").Append(string elm.Value).Append(", ")) |> string)
                |> sprintf "{ %s}" 
            | Array vs -> 
                sprintf "[| %s|]" ((StringBuilder(),vs) ||> Seq.fold (fun sb elm -> 
                    sb.Append(string elm).Append(", ")) |> string)
            | Table vs -> 
                match vs.Count with
                | 0 -> ""
                | 1 -> sprintf "{[ %s ]}" (Seq.head vs|> string) 
                | _ ->
                ((StringBuilder(),vs) ||> Seq.fold (fun sb kvp -> 
                    sb.AppendLine(sprintf "%*s = %s" (indent+4) kvp.Key (loop (indent+4) kvp.Value)))) 
                |> string |> fun str -> sprintf "{[\n%s %*s]}" str (indent-4) ""
        loop 4 value

    member private value.Display = value.ToString()

[<StructuredFormatDisplay "{Display}">]
type TOML() =
    let data            = table<string,Value>()
    let accessor        = table<string,Value ref>()
    let collision       = HashSet<string>()
    let arrayTables     = HashSet<string>()
    let lockedTables    = HashSet<string>()

    let keyName (key:string) = (key.Split '.').Last 
    let parentKey (key:string)  = 
        match key.LastIndexOf '.' with
        | -1 -> String.Empty | idx -> key.Substring(0,idx)

    /// Array of Tables key
    let aotkey key index =
        let parent = parentKey key
        let name = keyName key
        String.concat "." [parent;string index;name]


    let getNested (keyls:string list) =
        let rec loop ls (table:(string,Value)table) =
            match ls with
            | []        -> failwith "empty list is not a valid key"
            | [x]       -> table.[x]
            | hd::tl    -> 
                match table.[hd] with
                | Value.Table t -> loop tl t
                | v             -> failwithf "tried to use a key on a/
                                                value that isn't a table %s" (string v)
        loop keyls data

    let setup_insert (key:string) =
        // recursive ascent construction to ensure valid heirarchy for data insertion
        let rec loop key (tbl:table<_,_>) =
            let parent, name = parentKey key, keyName key
            let newtable = Value.Table tbl
            accessor.Add (key,ref newtable)
            // we've ascened to the top, now add all the data from below
            if parent  = "" && not (collision.Contains key) then 
                collision.Add key |> ignore
                data.Add (name, newtable)
            // found an existing parent, stop ascending
            elif accessor.ContainsKey parent then 
                match !accessor.[parent] with
                | Value.Table ptabl -> 
                    ptabl.Add(name, newtable)
                | _ -> failwith "can't add a table to anything but a table"
            else
                let ptbl = table<_,_>() 
                ptbl.Add(name,newtable) 
                loop parent ptbl
        loop key (table<_,_>()) 

    member toml.Item with get str  = !(accessor.[str])

    member toml.Item with get keyls = getNested keyls

    member toml.Add (key:string,value:Value,?AoT:bool)   = 
        let parent, name = parentKey key, keyName key 
        try
            match AoT,value with
            | Some true, Value.Table _  ->
                printfn "trying to add an array of tables"
                // Add an additonal table to an array of tables in the toplevel of the TOML datastructure
                if parent = String.Empty && arrayTables.Contains key then
                    match data.[name] with
                    | Value.Array ls  -> 
                        let akey = aotkey key ls.Length
                        accessor.Add (akey, ref value)
                        collision.Add akey |> ignore
                        data.[name] <- Value.Array (ls@[value])
                    | v -> failwithf "data for key `%s` was not an array, could not add %s" key (string v)
                // Create a new array of tables in the toplevel of the TOML datastructure
                elif parent = String.Empty then
                    arrayTables.Add key |> ignore
                    collision.Add   key |> ignore
                    //akey = aotkey key 0
                    let tval =  Value.Array [value]
                    accessor.Add (key,ref tval)
                    //accessor.Add (akey,ref value)
                    data.Add (name,tval)            
                // If the parent doesn't exist, ensure a chain up to the toplevel
                if not (accessor.ContainsKey parent)  then 
                    setup_insert parent 
                // If the array of tables doesn't exist add it to the parent
                if not (arrayTables.Contains key) then
                    let arr = Value.Array[value]
                    accessor.Add (key, ref arr)
                    arrayTables.Add key |> ignore
                    collision.Add key   |> ignore
                    match !accessor.[parent] with
                    | Value.Table tbl -> tbl.Add (name, arr)
                    | _ -> failwith "something went wrong"
                // Add an additonal table to an existing array of tables
                else
                    match !accessor.[parent], !accessor.[key] with
                    | Value.Table tbl,Value.Array arr -> 
                        let newarr = Value.Array (arr@[value])
                        accessor.[key] <- ref newarr
                        tbl.[name] <- newarr
                    | v -> failwithf "data for key `%s` was not an array, could not add %s" key (string v)
            | Some true,  v ->
                failwithf "Value for `%s` was not a table, could not add %s" key (string v)
            | _ -> 
                if collision.Add key = false then 
                    failwithf "key collsion on -- %s\n\
                               that key is already mapped to -\n %s" key (string !accessor.[key])
                else
                    collision.Add key |> ignore
                    accessor.Add(key, ref value)
                    if parent = String.Empty then 
                        data.Add (name,value) 
                    else
                        match !accessor.[parent], value with 
                        | Value.Table ptbl, Value.Table _ -> 
                            if lockedTables.Contains parent then 
                                failwithf "tried to add a table that already has elements other than tables"
                            else 
                                ptbl.Add(name,value)                        
                        | Value.Table ptbl, _ -> 
                            lockedTables.Add parent |> ignore
                            ptbl.Add(name,value)

                        | Value.Array ls, _ -> 
                            match List.last ls, value with
                            | Value.Table tbl,Value.Table vtbl when ls <> [] ->
                                let pkey = aotkey parent (ls.Length-1)
                                lockedTables.Add(aotkey parent (ls.Length-1)) |> ignore
                                accessor.Add(pkey+"."+name , ref value)
                                tbl.Add(name,value)
                            | Value.Table tbl,_ when ls <> [] ->
                                accessor.Add(aotkey key (ls.Length-1), ref value)
                                tbl.Add(name,value)
                            | inv ->
                            
                                failwithf "Keys can only be added to arrays of tables\- \n %s" (string inv)

                        | inv, _ -> failwithf "tried to add a key & value to an \
                                               element that can't store them - \n %s" (string inv)
        with
        | e ->  let parent = if parent = String.Empty then "toplevel" else parent
                printfn "Failed trying to add %s to %s\n" name  parent  
                printfn "Toml data at failstate --\n %A" data
                raise e    

    member private toml.Display = toml.ToString()
    override toml.ToString() =

        let sb = StringBuilder () in data |> Seq.iter (fun kvp -> 
            match kvp.Key,kvp.Value with
            | key, (Value.Table tbl) when tbl.Count = 0 ->
                sb.AppendLine(sprintf "{[ %s ]}" key)|> ignore
            | _ -> 
                let str = sprintf "%s = %s" kvp.Key (string kvp.Value)
                sb.AppendLine str |> ignore) 
        sb.Insert (0,'\n') |> string
    //

