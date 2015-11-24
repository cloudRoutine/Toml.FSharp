[<AutoOpen>]
module TomlFs.Prelude
open System
(*| Helpers |*)

/// Compose predicates with `&&`
let inline (|&|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x && pred2 x

/// Compose predicates with `||`
let inline (|?|) (pred1:'a->bool) (pred2:'a->bool) = fun x -> pred1 x || pred2 x



[<RequireQualifiedAccess>] 
module  List =
    let inline last ls =
        let rec loop ls =
            match ls with
            | x::[] -> x | _::tl -> loop tl | [] -> failwith "empty list has no last member"
        loop ls

[<RequireQualifiedAccess>] 
module String =
    /// checks if the front and back of a string match the provided
    /// 'bookend' substrings
    let bookends (front:string) (back:string) (str:string) =
        let flen, blen, slen = front.Length, back.Length, str.Length
        if  flen > slen  || blen > slen || flen + blen > slen 
         || isNull front || isNull back || isNull str then false 
        elif String.Empty = front && String.Empty = back then true else
        str.Substring (0,front.Length) = front &&
        str.Substring (str.Length-back.Length,back.Length) = back

