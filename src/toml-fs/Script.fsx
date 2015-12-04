#r @"../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../../packages/FParsec/lib/net40-client/FParsec.dll"
//#r @"bin/release/toml-fs.dll"
#load "Prelude.fs"
      "AST.fs"
      "Parsers.fs"

open System 
open FParsec
open TomlFs.Prelude
open TomlFs.AST
open TomlFs.Parsers


[<AutoOpen>] 
module  basicdata =
    let date1 = "1979-05-27T07:32:00Z"
    let date2 = "1979-05-27T00:32:00-07:00"
    let date3 = "1979-05-27T00:32:00.999999-07:00"
    let date4 = "1979-05-27T07:32:00"
    let date5 = "1979-05-27T00:32:00.999999"
    let date6 = "1979-05-27"

    let int0 = "1_000"  
    let int1 = "5_349_221" 
    let int2 = "1_2_3_4_5" 
    let int3 = "0_2_3_4_5" 
    let int4 = "72345" 

    let flt0 = "9._224_617  .445_991_228_313"
    let flt1 = "+1.0"
    let flt2 = "3.1415"
    let flt3 = "-0.01"
    let flt4 = "5e+22"
    let flt5 = "1e6"
    let flt6 = "-2E-2"
    let flt7 = "6.626e-34"

let table0 = """[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates
"""

let table1="""[database]
connection_max = 5000
server = "192.168.1.1"   
ports = [ 8001, 8001, 8002 ]
enabled = true
"""

[<AutoOpen>] 
module  toml_samples =
    let toml0 = """

    # This is a TOML document.

    title = "TOML Example"

    [owner]
    name = "Tom Preston-Werner"
    dob = 1979-05-27T07:32:00-08:00 # First class dates

    [database]
    server = "192.168.1.1"
    ports = [ 8001, 8001, 8002 ]
    connection_max = 5000
    enabled = true

    [servers]

      # Indentation (tabs and/or spaces) is allowed but not required
      [servers.alpha]
      ip = "10.0.0.1"
      dc = "eqdc10"

      [servers.beta]
      ip = "10.0.0.2"
      dc = "eqdc10"

    [clients]
    data = [ ["gamma", "delta"], [1, 2] ]

    # Line breaks are OK when inside arrays
    hosts = [
      "alpha",
      "omega"
    ]


    """

    let toml1 = """

    # This is a TOML document. Boom.

    title = "TOML Example"

    [owner]
    name = "Tom Preston-Werner"
    organization = "GitHub"
    bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
    dob = 1979-05-27T07:32:00Z # First class dates? Why not?

    [database]
    server = "192.168.1.1"
    ports = [ 8001, 8001, 8002 ]
    connection_max = 5000
    enabled = true


    """

    let toml2 = """

    # This is a TOML document. Boom.

    title = "TOML Example"

    [owner]
    name = "Tom Preston-Werner"
    organization = "GitHub"
    bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
    dob = 1979-05-27T07:32:00Z # First class dates? Why not?

    [database]
    server = "192.168.1.1"
    ports = [ 8001, 8001, 8002 ]
    connection_max = 5000
    enabled = true

    [servers]

      # You can indent as you please. Tabs or spaces. TOML don't care.
      [servers.alpha]
      ip = "10.0.0.1"
      dc = "eqdc10"

      [servers.beta]
      ip = "10.0.0.2"
      dc = "eqdc10"
      country = "中国" # This should be parsed as UTF-8

    [clients]
    data = [ ["gamma", "delta"], [1, 2] ] # just an update to make sure parsers support it

    # Line breaks are OK when inside arrays
    hosts = [
      "alpha",
      "omega"
    ]

    # Products

      [[products]]
      name = "Hammer"
      sku = 738594937

      [[products]]
      name = "Nail"
      sku = 284758393
      color = "gray"

    """

    let toml3 = """

    # Test file for TOML
    # Only this one tries to emulate a TOML file written by a user of the kind of parser writers probably hate
    # This part you'll really hate

    [the]
    test_string = "You'll hate me after this - #"          # " Annoying, isn't it?

        [the.hard]
        test_array = [ "] ", " # "]      # ] There you go, parse this!
        test_array2 = [ "Test #11 ]proved that", "Experiment #9 was a success" ]
        # You didn't think it'd as easy as chucking out the last #, did you?
        another_test_string = " Same thing, but with a string #"
        harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
        # Things will get harder
    """

    let toml4 = """

    # Test file for TOML
    # Only this one tries to emulate a TOML file written by a user of the kind of parser writers probably hate
    # This part you'll really hate

    [the]
    test_string = "You'll hate me after this - #"          # " Annoying, isn't it?

        [the.hard]
        test_array = [ "] ", " # "]      # ] There you go, parse this!
        test_array2 = [ "Test #11 ]proved that", "Experiment #9 was a success" ]
        # You didn't think it'd as easy as chucking out the last #, did you?
        another_test_string = " Same thing, but with a string #"
        harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
        # Things will get harder
    
            [the.hard.bit#]
            what? = "You don't think some user won't do that?"
            multi_line_array = [
                "]",
                # ] Oh yes I did
                ]

    """

    let toml5 = """

        [[fruit]]
      name = "apple"

      [fruit.physical]
        color = "red"
        shape = "round"

      [[fruit.variety]]
        name = "red delicious"

      [[fruit.variety]]
        name = "granny smith"

    [[fruit]]
      name = "banana"

      [[fruit.variety]]
        name = "plantain"
    """

    let toml6 = """
    
      [[products]]
      name = "Hammer"
      sku = 738594937

      [[products]]
      name = "Nail"
      sku = 284758393
      color = "gray"

      [[products]]
      name = "Hammer"
      sku = 738594937

      [[products]]
      name = "Nail"
      sku = 284758393
      color = "gray"
    """


let prun psr str = run psr str |> printfn "%A"
let parseString parser str = runParserOnString parser () "toml parser test" str;;


[<AutoOpen>] 
module simple_tests =
    let runtests = false

    if runtests then
        [   int0
            int1
            int2
            int3
            int4    ]    
        |> List.iter (prun pInt64_toml)

        [   flt0
            flt1
            flt2
            flt3
            flt4
            flt5
            flt6
            flt7    ]
        |> List.iter (prun pFloat_toml)

        [
            date1
            date2
            date3
            date4
            date5
            date6   ] 
        |> List.iter (prun pDateTime_toml)
;;

[<AutoOpen>] 
module complex_tests =
    let run_complex = true

    if run_complex then
        prun toml_inlineTable  "{ one = 1, two = 2, three = 3}"
        prun toml_string "\"hello\""
        prun toml_array  "[   \"hello\", \"watup\", \"yo\" ] "
        prun toml_array """[ [1,2,3], [1.0,2.0,3.0], ["a","b","c"], ]""" 
        prun toml_array  "[ 22.04 , 234.00, 23_4.304]" 
        prun toml_array "[ { x = 1, y = 2, z = 3 }, { x = 7, y = 8, z = 9 }, { x = 2, y = 4, z = 8 } ]" 
       // prun parse_toml_table table0 
      //  prun parse_toml_table table1 
        ;;



//
//[
//    toml0
//    toml1
//    toml2
//    toml3
//    toml4
//    toml5   
//    toml6   
//] |> fun x ->
//    x |> List.iter (prun parse_to_print)
//    x |> List.iter (prun parse_toml_table)

#time "on"

prun parse_toml_table toml0;;
prun parse_toml_table toml1;;
prun parse_toml_table toml2;;
prun parse_toml_table toml3;;
prun parse_toml_table toml4;;
prun parse_toml_table toml5;; 
prun parse_toml_table toml6;; 
        

//parseString basic_string "\"\\rline1\\nline2\\nline3\\rline4\"";;


//parseString basic_string "\"?\"";;

