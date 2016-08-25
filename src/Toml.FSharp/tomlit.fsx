System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__
#r @"../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsec.dll"
//#r @"bin/release/toml-fs.dll"
#load "Prelude.fs"
      "AST.fs"
      "Parsers.fs"
open FParsec
open TomlFSharp.Prelude
open TomlFSharp.AST
open TomlFSharp.Parsers


type DebugType =
    | None | PdbOnly | Full
type Net  = V_4_5 | V4_5_1 | V4_6 | V4_6_1 | V4_6_2 
type NetcoreApp  = V1_0
type NetStandard =  V1_0 | V1_1 | V1_2 | V1_3 | V1_4 | V1_5 | V1_6

// Framework Id
type  FrameworkTarget = Net of Net | NetStandard of NetStandard | NetcoreApp of NetcoreApp

type Configuration = {
    FrameworkTarget : FrameworkTarget
    Tailcalls : bool
    WarningsAsErrors : bool
    Constants : string []
    DebugType : DebugType
    DebugSymbols : bool
    Optimize : bool
    Prefer32bit : bool
    WarningLevel : int
    OutputPath : string
    DocumentationFile : string
    NoWarn : int []
    OtherFlags : string []
}

let parsetoml filepath =
    match runParserOnFile parse_toml_table  () filepath (System.Text.UTF8Encoding.UTF8) with
    | ParserResult.Failure(a,b,c) -> failwithf "%s" a
    | ParserResult.Success(res,_,_) -> res

let prunfile psr filepath =  
    runParserOnFile psr () filepath (System.Text.UTF8Encoding.UTF8)
    |> printfn "%A"
;;
prunfile parse_toml_table "testproject.toml"    
;;
let tbl = parsetoml "testproject.toml" ;;   

tbl.SubTables() |> Seq.iter(printfn "%A");;

tbl.ToString();;