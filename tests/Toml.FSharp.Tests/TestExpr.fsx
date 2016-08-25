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



let x  = "\"W>⤃╍楮忇粥菀灴裸.℄埧玛M㙟巑墹湳ḃ馜胐4疈螕狁ん楏\"⩲摽鴹㛾幜GㆀBЁ+敪黒㟫彉饔㌙๎\" = +266_55_45_821407"
parseString toml_item x;;
x.ToCharArray();;


//let rng = Random()
//for _ in 0..50 do 
//    genKeyValPair
//    |> Gen.eval 5 (Random.StdGen(rng.Next(),rng.Next())) |> printfn "%s\n"
//


// Running an FsCheck Generator in FSI 
open System
let rng = Random()
;;
for _ in 0..10 do 
    Gen.elements(['A'..'Z']@['a'..'z']@['0'..'9']@['_'])
    |> Gen.arrayOf |> lenAbove 3 |> Gen.map String
    |> Gen.listOf  |> lenAbove 2 
    |> Gen.map (String.concat ".") 
    |> Gen.eval 5 (Random.StdGen(rng.Next(),rng.Next())) |> printfn "%s\n"
