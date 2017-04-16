namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Toml.FSharp")>]
[<assembly: AssemblyProductAttribute("Toml.FSharp")>]
[<assembly: AssemblyDescriptionAttribute("FParsec TOML Parser for F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
