module LogicalTable.Types.Ternary

open System

type Ternary =
    | False
    | True
    | DontCare

    static member Parse str =
        match str with
        | "False" -> False
        | "True" -> True
        | "DontCare" -> DontCare
        | null -> raise (ArgumentNullException(nameof str))
        | _ -> raise (FormatException($"Invalid brush string: '{str}'"))
