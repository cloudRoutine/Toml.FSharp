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

let RFC3999DateTime (dateTime:DateTime) = dateTime.ToString("yyyy-MM-dd'T'HH:mm:ssZ")
RFC3999DateTime (DateTime(2000,10,22,9,44,14,35));;
//let x = "\"㭀醦䷛觚᧑锕෶ \""
//parseString basic_string x;;
//x.ToCharArray();;
//let ms = "\"\"\"_______\"\"\"";;
//parseString multi_string ms;;
let x = "\"M啖W䛛:\".\"蝏\".\"撼᳢园ゥ\".\"S䉂竾ᓷ磲\""
parseString pQuoteKey x;;
x.ToCharArray();;

(* Running an FsCheck Generator in FSI

let rng = Random()
for _ in 0..10 do 
    Gen.elements(['A'..'Z']@['a'..'z']@['0'..'9']@['_'])
    |> Gen.arrayOf |> lenAbove 3 |> Gen.map String
    |> Gen.listOf  |> lenAbove 2 
    |> Gen.map (String.concat ".") 
    |> Gen.eval 5 (Random.StdGen(rng.Next(),rng.Next())) |> printfn "%s\n"

*)