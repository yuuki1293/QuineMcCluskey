module Cell

open System

type Cell =
    | False
    | True
    | Both
    override this.ToString() =
        match this with
        | False -> "0"
        | True -> "1"
        | Both -> "-"
    
    static member from digit (str: string) =
        str.PadLeft (digit, '0')
        |> Seq.map (function
            | '0' -> False
            | '1' -> True
            | '-' -> Both
            | e -> raise (Exception($"Unexpected char '{e}'")))
        |> Seq.toList