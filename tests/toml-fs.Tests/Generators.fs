[<AutoOpen>]
module TomlFs.Tests.Generators
open System
open System.Text
open FsCheck


(*|-------------------|*)
(*| String Generators |*)
(*|-------------------|*)


let genCtrlSeq =
    [0x00..0x1f] |> (Gen.elements>>Gen.map(char>>fun c -> [|'\\';c|]))

let genEscSeq =
    ['b';'t';'n';'f';'r';'"';'\\']
    |> (Gen.elements>>Gen.map(fun c -> [|'\\';c|]))

/// generate a unicode char that doesn't need to be escaped
let genUnicode =
    let make = Gen.choose>>Gen.map(fun x ->[| char x |])
    Gen.frequency 
        [   2       , (0x20, 0x21   ) |> make
            270     , (0x23, 0x3e   ) |> make
            270     , (0x40, 0x5b   ) |> make
            4000    , (0x5d, 0x9fff ) |> make ]

let litset = // skip \n 0x10, \r 0x13, `'` 0x27
    [100,[0x00..0x09]@[0x11;0x12]@[0x14..0x26]@[0x28..0x1f]
        |> (Gen.elements>>Gen.map (fun x -> [| char x |]))]

let multilitset =
    [100, [0x00..0x1f]|> (Gen.elements>>Gen.map (fun x -> [| char x |]))]

let genscaffold sqs delim = 
    let genArr = Gen.frequency sqs |> Gen.arrayOf
    Gen.map2
        (fun arr uc -> 
            let flat =  Array.concat arr
            let quoted = Array.concat [delim;flat;uc;delim]
            String quoted) genArr genUnicode 

let basicSet = [32,genCtrlSeq; 7,genEscSeq ;4000,genUnicode] 
let multiSet = [100,gen { return [|'\n'|]}]@basicSet

let genBasicString    = genscaffold basicSet [|'\"'|]
let genMultiString    = genscaffold multiSet [|'\"';'\"';'\"'|]
let genLiteralString  = genscaffold litset   [|'\''|]
let genMultiLitString = genscaffold multiSet [|'\'';'\'';'\''|]

let basic_string_gen    = Arb.fromGen genBasicString 
let multi_string_gen    = Arb.fromGen genMultiString
let literal_string_gen  = Arb.fromGen genLiteralString
let multi_lit_string_gen = Arb.fromGen genMultiLitString

type BasicString = static member String () = basic_string_gen



// try generating \uXXXX and \UXXXXXXXX as char arrays and then convert to string
// make sure they're out of the range of contorl chars

(* 

* String Generators 
-------------------
    - Basic
    - Multiline Basic
    - Literal
    - Multiline Literal


* Comment Generator
-------------------
    # followed by text
    end with `\n`

* Integer Generator
-------------------
    0 alone
    can start with + or -
    [1-9] first digit
    `_` but only between digits e.g. 200_000
    64 bit (signed long) range expected 
        (−9,223,372,036,854,775,808 to 9,223,372,036,854,775,807)


* Float Generator
-----------------


* Bare Keys
------------
Bare keys may only contain letters, numbers, underscores, and dashes (A-Za-z0-9_-). 
Note that bare keys are allowed to be composed of only digits, e.g. 1234. 

* Quoted Keys
-------------
Basic strings interspersed with `.`


*)


