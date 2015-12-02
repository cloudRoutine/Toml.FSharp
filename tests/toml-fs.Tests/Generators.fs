[<AutoOpen>]
module TomlFs.Tests.Generators
open System
open System.Text
open FsCheck


(*|-------------------|*)
(*| String Generators |*)
(*|-------------------|*)


let genCtrlSeq =
    [for hex in 0x00..0x1f -> char hex]
    |> (Gen.elements>>Gen.map(fun c -> [|'\\';c|]))

let genEscSeq =
    ['b';'t';'n';'f';'r';'"';'\\']
    |> (Gen.elements>>Gen.map(fun c -> [|'\\';c|]))


let genUnicode =
    let make = Gen.choose>>Gen.map(fun x ->[| char x |])
    Gen.frequency 
        [   2       , (0x20, 0x21   ) |> make
            270     , (0x23, 0x3e   ) |> make
            270     , (0x40, 0x5b   ) |> make
            4000    , (0x5d, 0x9fff ) |> make ]

// 0x22 is `"`


let genBasicString = 
    let genArr = Gen.frequency [32,genCtrlSeq; 7,genEscSeq ;4000,genUnicode]|> Gen.arrayOf
    Gen.map2
        (fun arr uc -> 
            let flat =  Array.concat arr
            let quoted = Array.concat [[|'\"'|];flat;uc;[|'\"'|]]
            String quoted) genArr genUnicode 

let basic_string_gen = Arb.fromGen genBasicString

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


