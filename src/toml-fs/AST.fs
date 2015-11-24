module TomlFs.AST
#nowarn "62"
open System
open System.Text
open System.Collections.Generic

type ('key,'value) Dictionary with
    member self.TryAdd (key,value) =
        if self.ContainsKey key then false else self.Add (key,value); true

    member self.AddMany xs = Seq.iter self.Add xs
        
    member self.AddOnlyNewKeys ls = ls |> List.iter (self.TryAdd>>ignore)


let inline private seqstr xs (modstr:string) =
    let sb = System.Text.StringBuilder()
    xs |> Seq.iter (fun x -> sb.Append(string x).Append(",").Append(modstr)|>ignore)
    string sb  

type ('k,'v) table = ('k,'v) Dictionary

and [<StructuredFormatDisplay "{Display}">]
    TOML = 
    TOML of (string,Value) table

        override toml.ToString() =
            let (TOML tbl) = toml
            let sb = StringBuilder () in tbl |> Seq.iter (fun kvp -> 
                match kvp.Key,kvp.Value with
                | key, (Value.Table tbl) when tbl.Count = 0 ->
                    sb.AppendLine(sprintf "{[ %s ]}" key)|> ignore
                | _ -> 
                    let str = sprintf "%s = %s" kvp.Key (string kvp.Value)
                    sb.AppendLine str |> ignore) 
            sb.Insert (0,'\n') |> string

        member private toml.Display = toml.ToString()
        member private toml.Table   = let (TOML tbl) = toml in tbl

        member toml.Item 
            with get str   = toml.Table.[str]
            and  set str v = toml.Table.[str] <- v

        member toml.Add (k,v)   = 
            try
                toml.Table.Add (k,v)
            with _ ->  
                printfn "failed trying to add %s, %s" k (string v)
                printfn "TOML Parsed -\n%s" (string toml)

        member toml.TryAdd      = toml.Table.TryAdd
        member toml.ContainsKey = toml.Table.ContainsKey
        member toml.AddMany     = toml.Table.AddMany

and [<RequireQualifiedAccess>] 
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
    | InlineTable   of (string, Value) table    // "unwrap" keys for table storage
    | Table         of (string, Value) table     // should this be a value?
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
            | ArrayOfTables vs ->
                ((StringBuilder(),vs) ||> Seq.fold (fun sb arr -> 
                    sb.AppendLine(sprintf "%*s" (indent+4) (loop (indent+4)(Value.InlineTable arr)) ))) 
                |> string |> fun str -> sprintf "[|\n%*s %*s|]\n" indent str (indent-4) ""
        loop 4 value

    member private value.Display = value.ToString()



