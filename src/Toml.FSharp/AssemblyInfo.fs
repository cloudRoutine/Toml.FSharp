namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("toml-fs")>]
[<assembly: AssemblyProductAttribute("toml-fs")>]
[<assembly: AssemblyDescriptionAttribute("FParsec TOML Parser for F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
