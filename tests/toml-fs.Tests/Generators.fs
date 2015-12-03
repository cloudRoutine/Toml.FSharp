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

let inline genCharr x = x |> Gen.map (fun x -> [|char x|])

/// generate a unicode char that doesn't need to be escaped
let genUnicode =
    let make = Gen.choose>>genCharr
    Gen.frequency 
        [   2       , (0x20, 0x21   ) |> make
            270     , (0x23, 0x3e   ) |> make
            270     , (0x40, 0x5b   ) |> make
            4000    , (0x5d, 0x9fff ) |> make]

let lit_set = // skip \n 0x10, \r 0x13, `'` 0x27
    [100,[0x00..0x09]@[0x11;0x12]@[0x14..0x26]@[0x28..0x1f]
        |> (Gen.elements>>genCharr)]

let multi_lit_set =
    [100, [0x00..0x1f]|> (Gen.elements>>genCharr)]

let gen_scaffold sqs delim = 
    let genArr = Gen.frequency sqs |> Gen.arrayOf
    Gen.map2 (fun arr uc -> 
        let flat    = Array.concat arr
        let quoted  = Array.concat [delim;flat;uc;delim]
        String quoted) genArr genUnicode 

let basic_set = [32,genCtrlSeq; 7,genEscSeq ;4000,genUnicode] 
let multi_set = [100,gen { return [|'\n'|]}]@basic_set

let genBasicString    = gen_scaffold basic_set       [|'\"'|]
let genMultiString    = gen_scaffold multi_set       [|'\"';'\"';'\"'|]
let genLiteralString  = gen_scaffold lit_set         [|'\''|]
let genMultiLitString = gen_scaffold multi_lit_set   [|'\'';'\'';'\''|]

let genTomlString = 
    Gen.oneof [genBasicString; genMultiString; genLiteralString; genMultiLitString]


let basic_string_arb     = Arb.fromGen genBasicString 
let multi_string_arb     = Arb.fromGen genMultiString
let literal_string_arb   = Arb.fromGen genLiteralString
let multi_lit_string_arb = Arb.fromGen genMultiLitString
let toml_string_arb      = Arb.fromGen genTomlString


// try generating \uXXXX and \UXXXXXXXX as char arrays and then convert to string
// make sure they're out of the range of contorl chars


(*|-------------------------|*)
(*| Simple Value Generators |*)
(*|-------------------------|*)

// 64 bit (signed long) range expected (−9,223,372,036,854,775,808 to 9,223,372,036,854,775,807).

let inline lenAbove num (a:'a[]) = a.Length > num
let inline lenBelow num (a:'a[]) = a.Length < num

    
let gen_digit       = ['0'..'9'] |> Gen.elements 
let gen_first_digit = ['+';'-']@['1'..'9'] |> Gen.elements 
let gen_mid_digits  = 
        let uscore  = (gen_digit, gen_digit) ||> Gen.map2 (fun a b -> [|a;'_';b|])
        Gen.oneof [uscore; genCharr gen_digit  ]
        |> (Gen.listOf >> Gen.map Array.concat)
        |> Gen.suchThat (lenAbove 1)


let genTomlInt = 
    Gen.map2  (fun a b -> Array.concat [a;b])
        (genCharr gen_first_digit) gen_mid_digits
    |> Gen.suchThat (lenBelow 20)
    |> Gen.map String

//Double.MinValue -1.797693135e+308
//Double.MaxValue 1.797693135e+308
 //              "1.79769313486232E+308";;


//let genTomlFloat = 
//    let num = (genCharr gen_first_digit, gen_mid_digits, gen_mid_digits)
//                |||> Gen.map3 (fun digi0 midA midB -> Array.concat [digi0; midA; [|'.'|]; midA])
//    let zerod = Gen.map (fun arr -> Array.concat [[|'0';'.'|];arr])  gen_mid_digits
//                |> Gen.suchThat (lenBelow 8)
//    let expn  = Gen.map3 (fun e digi0  digits -> Array.concat [e;digi0;digits]  )
//                    (Gen.elements['e';'E'] |> genCharr)
//                    (Gen.elements['+';'-'] |> genCharr)
//                    (gen_mid_digits|> Gen.suchThat 
//                        (fun x ->   let exp = (String x).Replace("_","")|> int
//                                    exp < 308 && exp > -307)) //308 is max base 10 exponent
//                |> Gen.suchThat (lenBelow 6)
//    let numExpn   = Gen.map2 Array.append (num|>Gen.suchThat (lenBelow 6))   expn
//    let zerodExpn = Gen.map2 Array.append zerod expn
//    Gen.oneof [ num; zerod; numExpn; zerodExpn ] 
//    |> Gen.suchThat (lenBelow 30)
//    |> Gen.map String

let rng = System.Random()

let randomInsert (num:float) =
    let str = string num
    let dot = str.IndexOf '.' 
    let rec loop (idx,newStr:string) =
        if  idx <> dot   && idx <> dot+1 && idx <> dot-1 then
            if idx > newStr.Length-2 then newStr else
            loop (idx + rng.Next(2,4),  newStr.Insert(idx,"_"))
        else
            loop (idx + rng.Next(2,4),newStr)
    loop (2,str) 


let genTomlFloat = 
    Arb.generate<float> |> Gen.suchThat (fun flt -> 
        flt <> Double.NegativeInfinity &&
        flt <> Double.PositiveInfinity &&
        flt <> Double.NaN && 
        flt < Double.MaxValue &&
        flt > Double.MinValue) |> Gen.map randomInsert





let toml_int_arb    = Arb.fromGen genTomlInt
let toml_float_arb  = Arb.fromGen genTomlFloat

(* 


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


