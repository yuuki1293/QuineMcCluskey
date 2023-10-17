module Table

open Cell
open Row

type Table =
    /// digit
    /// label1 000000
    /// label2 001000 *
    static member from(str: string) =
        let splitted = str.Split("\n")
        
        if splitted.Length <> 3 then
            Error("Invalid syntax.")
        else
            let digit = splitted.[0] |> int
            seq {
                for row in splitted |> Seq.tail do
                    match str.Split(" ") |> Array.toList with
                    | labelName::num::"*"::[_] ->
                        let data =
                            num
                            |> Cell.from digit
                        let label =
                            [labelName, true]
                            
                        yield {data = data; label = label; isEnd = false}
                    | labelName::num::[_] ->
                        let data =
                            num
                            |> Cell.from digit
                        let label =
                            [labelName, false]
                            
                        yield {data = data; label = label; isEnd = false}
                    | _ -> ()
            }
            |> Ok