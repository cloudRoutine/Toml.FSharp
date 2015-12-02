module TomlFs.Tests.Generators

open FsCheck



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


* Quoted Keys
-------------



*)



