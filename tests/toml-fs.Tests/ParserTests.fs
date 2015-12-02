#if INTERACTIVE
#r @"../../packages/NUnit/lib/net45/nunit.framework.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsec.dll"
#r @"../../packages/FsCheck/lib/net45/FsCheck.dll"
#r @"../../src/toml-fs/bin/release/toml-fs.dll"
#load   "Prelude.fs"
        "Generators.fs"
#else
module TomlFs.Tests.ParserTests
#endif


open NUnit.Framework
open FsCheck
open FParsec 
open TomlFs.Prelude
open TomlFs.Parsers
open TomlFs.Tests.Prelude
open TomlFs.Tests.Generators


let [<Test>] ``parses all basic strings`` () =
    Prop.forAll basic_string_gen (fun (str:string) ->
        (str.Length > 3) ==>
            match parseString basic_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 
    |> fun x -> 
        Check.One({Config.QuickThrowOnFailure with  
                    MaxTest     = 25000 
                    StartSize   = 50    
                    EndSize     = 2000  
                    }, x )








#if INTERACTIVE
``parses all basic strings`` ()
#endif

