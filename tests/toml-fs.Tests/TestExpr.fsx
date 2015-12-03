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
RFC3999DateTime (DateTime(2000,10,22,9,44,14,35))
//let x = "\"㭀醦䷛觚᧑锕෶ \""
//parseString basic_string x;;
//x.ToCharArray();;
//let ms = "\"\"\"_______\"\"\"";;
//parseString multi_string ms;;

parseString toml_array "[ '%	\"	糒', \"L綮+㸥MᇵI爏1䗟纛ᡠ㾾⾏\", \"\"\"鵋
> A䩔聣[䐕E᜹#矿)䬣蒋ᶤ䔂缍ᣒ冎瘕\"\"\", '!&!幒', '''校''', '䱿', \"\"\"豥埊豉
叒屩俥托珞橄
各悅@#衇⳪晒鼎㥶䷀\"\"\", \"\"\"〼伶掏瓵@焒毐葔ᥰ卻賣◼䵚蛂K姦腄ᬉ唔跐ᖋ仳裾∗孿苝᯶嘁A\"\"\", '%'', '''";;


