open System
open System.Collections.Generic

type Tabler =
    {   DateTimes       : Map<string, DateTime>
        Ints            : Map<string, int>
        Strings         : Map<string, string>
        Floats          : Map<string, float>
        Bools           : Map<string, bool>
        DateTimeLists   : Map<string, DateTime list>
        IntLists        : Map<string, int list>
        StringLists     : Map<string, string list>
        FloatLists      : Map<string, float list>
        BoolLists       : Map<string, bool list>
        TableLists      : Map<string, Tabler list>
        SubTables       : Map<string, Tabler>
    }
    static member Empty =
        {   DateTimes     = Map.empty
            Ints          = Map.empty
            Strings       = Map.empty
            Floats        = Map.empty
            Bools         = Map.empty
            DateTimeLists = Map.empty
            IntLists      = Map.empty
            StringLists   = Map.empty
            FloatLists    = Map.empty
            BoolLists     = Map.empty
            TableLists    = Map.empty
            SubTables     = Map.empty
        }

type Config =
    {   People  : string list
        Started : DateTime
        Amp     : float
        Hungry  : bool
    }