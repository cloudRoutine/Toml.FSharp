module TomlFs.AST
#nowarn "62"
open System
open System.Collections.Generic

type path = string
type ('k,'v) table = ('k,'v) Dictionary

type tomlseq = path * (string*Value) seq

and toml = path * (string,Value) table

and [<RequireQualifiedAccess>] 
    Key = 
    | Bare      of string
    | Quoted    of string
    override key.ToString () =
        match key with
        | Bare k   -> k
        | Quoted k -> k 

and [<RequireQualifiedAccess>] 
    Value =
    | String       of string
    | Int          of int64
    | Float        of float
    | Bool         of bool
    | DateTime     of DateTime
    | InlineTable  of (string, Value) table    // "unwrap" keys for table storage
    | Table        of (string, Value) table     // should this be a value?
    | Array        of Value list
    override value.ToString () =
        let inline seqstr xs =
            let sb = System.Text.StringBuilder()
            xs |> Seq.iter (fun x -> sb.Append(string x).Append(", ")|>ignore)
            string sb            
        match value with
        | String v       -> v
        | Int v          -> string v
        | Float v        -> string v
        | Bool v         -> string v
        | DateTime v     -> string v
        | InlineTable vs -> sprintf "{ %s }" (seqstr vs)
        | Array vs       -> sprintf "[ %s ]" (seqstr vs)
        | Table vs       -> sprintf "{| %s |}" (seqstr vs)


