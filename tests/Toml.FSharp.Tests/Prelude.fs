[<AutoOpen>]
module TomlFs.Tests.Prelude

open System
open FsCheck
open FParsec
open TomlFSharp.Parsers


let inline registerArb<'a>() = Arb.register<'a>() |> ignore

let parseString parser str = runParserOnString parser () "toml-fs string parser test" str