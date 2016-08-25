[<AutoOpen>]
module TomlFs.Tests.Generators
open System
open System.Text
open FsCheck


(*|-------------------|*)
(*| String Generators |*)
(*|-------------------|*)


let genCtrlSeq = // 0 - 31
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

let genMultiLitString = gen_scaffold multi_lit_set   [|'\'';'\'';'\''|]
let genLiteralString  =
    Gen.map (fun arr -> 
        let flat    = Array.concat arr
        let quoted  = Array.concat [[|'\''|];flat;[|'\''|]]
        String quoted) (Gen.frequency lit_set |> Gen.arrayOf)


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

let inline lenAbove num = Gen.suchThat (fun a -> (^a:(member Length:int)a) > num)
let inline lenBelow num = Gen.suchThat (fun a -> (^a:(member Length:int)a) < num)

    
let gen_digit       = ['0'..'9'] |> Gen.elements 
let gen_first_digit = ['+';'-']@['1'..'9'] |> Gen.elements 

let gen_mid_digits  = 
    let uscore  = (gen_digit, gen_digit) ||> Gen.map2 (fun a b -> [|a;'_';b|])
    Gen.oneof [uscore; genCharr gen_digit  ]
    |> (Gen.listOf >> Gen.map Array.concat) |> lenAbove 1


let genTomlInt = 
    Gen.map2  (fun a b -> Array.concat [a;b])
        (genCharr gen_first_digit) gen_mid_digits
    |> lenBelow 20 |> Gen.map String


let rng = System.Random ()

let randomInsert (num:float) =
    let str = string num
    let dot = str.IndexOf '.' 
    let rec loop (idx,newStr:string) =
        if  idx <> dot   && idx <> dot+1 && idx <> dot-1 then
            if idx > newStr.Length-2 then newStr else
            loop  (idx + rng.Next(2,4), newStr.Insert(idx,"_"))
        else loop (idx + rng.Next(2,4), newStr)
    loop (2,str) 


let genTomlFloat = 
    Arb.generate<float> |> Gen.suchThat (fun flt -> 
        flt <> Double.NegativeInfinity &&
        flt <> Double.PositiveInfinity &&
        flt <> Double.NaN       && 
        flt <  Double.MaxValue  &&
        flt >  Double.MinValue) 
        |> Gen.map randomInsert |> Gen.suchThat (fun s -> s.IndexOf '.' <> -1)


let genBool = Arb.generate<bool> |> Gen.map (fun x -> (string x).ToLower())

let RFC3999DateTime (dateTime:DateTime) = dateTime.ToString "yyyy-MM-dd'T'HH:mm:ssZ"

let full_datetime = Arb.generate<DateTime> |> Gen.map RFC3999DateTime

let genDateTime = 
    Gen.oneof [ full_datetime;
        (full_datetime |> Gen.map(fun x -> x.Substring(0,20)))]

let toml_int_arb        = Arb.fromGen genTomlInt
let toml_float_arb      = Arb.fromGen genTomlFloat
let toml_bool_arb       = Arb.fromGen genBool
let toml_datetime_arb   = Arb.fromGen genDateTime


let value_set =
   [genTomlInt
    genTomlFloat
    genBool
    genDateTime 
    genBasicString
    genLiteralString
    (Arb.generate<int>  |> Gen.map string) ]

let genArray =
    value_set |> List.map Gen.listOf
    |> Gen.oneof |> Gen.map (String.concat ", ")
        |> Gen.map (sprintf "[ %s ]")
        

let toml_array_arb  = Arb.fromGen genArray

let genValue = Gen.oneof (genArray::value_set)

let genBareKey = 
    Gen.elements(['A'..'Z']@['a'..'z']@['0'..'9']@['_'])
    |> Gen.arrayOf |> lenAbove 3 |> Gen.map String

let genBareTableKey =
    genBareKey |> Gen.listOf |> lenAbove 2 |> Gen.map (String.concat ".") 

let genQuoteTableKey =
    genBasicString |> Gen.listOf |> lenAbove 2 |> Gen.map (String.concat ".") 

let genKey = (Gen.oneof[genBareKey;genBasicString])
let genTableKey = (Gen.oneof[genBareTableKey;genQuoteTableKey])

let genKeyValPair = 
    Gen.map2 (fun key value -> String.Concat [|key;" = ";value|])
        genKey genValue

let toml_bareKey_arb         = Arb.fromGen genBareKey
let toml_bareTableKey_arb    = Arb.fromGen genBareTableKey
let toml_quoteTableKey_arb   = Arb.fromGen genQuoteTableKey
let toml_key_arb             = Arb.fromGen genKey
let toml_tableKey_arb        = Arb.fromGen genTableKey
let toml_item_arb            = Arb.fromGen genKeyValPair

