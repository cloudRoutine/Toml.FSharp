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

let longCheck x =
    Check.One
        ({ Config.QuickThrowOnFailure with  
            MaxTest = 10000; StartSize = 50; EndSize = 700}, x )


let midCheck x =
    Check.One
        ({ Config.QuickThrowOnFailure with  
            MaxTest = 3000; StartSize = 20; EndSize = 300}, x )

let [<Test>] ``parses all basic strings`` () =
    longCheck <|
    Prop.forAll basic_string_arb (fun (str:string) ->
        (str.Length > 3) ==>
            match parseString basic_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 


let [<Test>] ``parses all multi strings`` () =
    longCheck <|
    Prop.forAll multi_string_arb (fun (str:string) ->
        (str.Length > 6) ==>
            match parseString multi_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 


let [<Test>] ``parses all literal strings`` () =
    longCheck <|
    Prop.forAll literal_string_arb (fun (str:string) ->
        (str.Length > 3) ==>
            match parseString literal_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 


let [<Test>] ``parses all multi literal strings`` () =
    longCheck <|
    Prop.forAll multi_lit_string_arb (fun (str:string) ->
        (str.Length > 6) ==>
            match parseString multi_literal_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 



let [<Test>] ``unified string parser reads all toml string types`` () =
    midCheck <|
    Prop.forAll toml_string_arb (fun (str:string) ->
        (str.Length > 6) ==>
            match parseString toml_string str with
            | ParserResult.Success(_,_,_) -> true
            | ParserResult.Failure(_,_,_) -> false) 






#if INTERACTIVE
``parses all basic strings`` ()
``parses all multi strings`` ()
``parses all literal strings`` ()
``parses all multi literal strings`` () 
 ``parses all toml strings`` () 
#endif

