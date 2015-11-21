module TomlFs.AST
#nowarn "62"
open System
open System.Text
open System.Collections.Generic

let inline private seqstr xs (modstr:string) =
    let sb = System.Text.StringBuilder()
    xs |> Seq.iter (fun x -> sb.Append(string x).Append(",").Append(modstr)|>ignore)
    string sb  

type path = string
type ('k,'v) table = ('k,'v) Dictionary

type tomlseq = path * (string*Value) seq

and [<StructuredFormatDisplay "{Display}">]
    TOML = 
    TOML of (string,Value) table

        override toml.ToString() =
            match toml with
            | TOML tbl -> 
                let sb = System.Text.StringBuilder()
                tbl |> Seq.iter (fun kvp -> 
                    let str = sprintf "%s = %s" kvp.Key (string kvp.Value)
                    sb.AppendLine str |> ignore)
                "\n"+ string sb

        member private toml.Display = toml.ToString()

and [<RequireQualifiedAccess>] 
    Key = 
    | Bare      of string
    | Quoted    of string
    override key.ToString () =
        match key with
        | Bare k   -> k
        | Quoted k -> k 

and [<RequireQualifiedAccess>] 
    [<StructuredFormatDisplay "{Display}">]
    Value =
    | String       of string
    | Int          of int64
    | Float        of float
    | Bool         of bool
    | DateTime     of DateTime
    | InlineTable  of (string, Value) table    // "unwrap" keys for table storage
    | Table        of (string, Value) table     // should this be a value?
    | Array        of Value list
    | TableArray   of ((string, Value) table) list

    override value.ToString () =
        let rec loop (indent:int) (toml:Value) =
            match toml with
            | String v       -> v
            | Int v          -> string v
            | Float v        -> (string v)
            | Bool v         -> (string v)
            | DateTime v     -> (string v)
            | InlineTable vs -> 
                let str =
                    ((StringBuilder(),vs) ||> Seq.fold (fun sb elm -> 
                        sb.Append(elm.Key).Append(" = ").Append(string elm.Value).Append(", ")) |> string)
                sprintf "{ %s}" str
            | Array vs       -> 
                sprintf "[| %s|]" ((StringBuilder(),vs) ||> Seq.fold (fun sb elm -> 
                    sb.Append(string elm).Append(", ")) |> string)
            | Table vs       -> 
                if vs.Count = 1 then sprintf "{[ %s ]}" (Seq.head vs|> string) else
                let str = 
                    ((StringBuilder(),vs) ||> Seq.fold (fun sb kvp -> 
                        sb.AppendLine(sprintf "%*s = %s" (indent+4) kvp.Key (loop (indent+4) kvp.Value)))) 
                    |> string 
                sprintf "{[\n%s %*s]}" str (indent-4) ""
            | TableArray vs ->
                let str =
                    ((StringBuilder(),vs) ||> Seq.fold (fun sb arr -> 
                        sb.AppendLine(sprintf "%*s" (indent+4) (loop (indent+4)(Value.InlineTable arr)) ))) 
                    |> string 
                sprintf "[|\n%*s %*s|]\n" indent str (indent-4) ""
        loop 4 value

    member private value.Display = value.ToString()



