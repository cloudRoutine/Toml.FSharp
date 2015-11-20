module TomlFs.AST
#nowarn "62"
open System
open System.Collections.Generic

type path = string
type ('k,'v) table = ('k,'v) Dictionary

type tomlseq = path * item seq
and toml = path * (Key,Value) Dictionary 

and item  = Key * Value

and [<RequireQualifiedAccess>] 
    Key = 
    | Bare      of string
    | Quoted    of String
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
    | InlineTable  of (Key,Value) table
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



