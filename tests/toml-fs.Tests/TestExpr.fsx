#r @"../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsec.dll"
#r @"../../packages/FsCheck/lib/net45/FsCheck.dll"
#r @"../../src/toml-fs/bin/release/toml-fs.dll"
#load   "Prelude.fs"
        "Generators.fs"


open FsCheck
open FParsec 
open TomlFs
open TomlFs.Tests.Prelude
open TomlFs.Tests.Generators
open TomlFs.Parsers



//Prop.forAll basic_string_gen (fun (str:string) ->
//    (str.Length > 2) ==>
//        match parseString basic_string str with
//        | Success(_,_,_) -> true
//        | _ -> false) |> fun x -> 
//        Check.One({Config.QuickThrowOnFailure with  
//                    MaxTest     = 25000 
//                    StartSize   = 50    
//                    EndSize     = 2000  
//                    }, x )
//
//;;
open System
open FsCheck
open FParsec 
open TomlFs
open TomlFs.Tests.Prelude
open TomlFs.Tests.Generators
open TomlFs.Parsers

//let x = "\"㭀醦䷛觚᧑锕෶ \""
//parseString basic_string x;;
//x.ToCharArray();;
let ms = "\"\"\"_______\"\"\"";;
parseString multi_string ms;;

parseString multi_literal_string "'''_______'''"
//
//let s = String([|'"';'\\';'\022';'"'|]);;
//s.ToCharArray();;
//
//parseString basic_string s;;


