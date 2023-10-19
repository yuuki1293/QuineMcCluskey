module Table

open FSharpPlus
open Cell
open Row

type Table =
    /// <summary>Create Row seq from String</summary>
    /// <param name="str">input string</param>
    /// <returns>Row sequence</returns>
    /// <example>
    /// <code>
    /// <c>
    /// """6
    /// label1 000000
    /// label2 001000 *
    /// // Ignore here.
    /// """
    /// |> Table.from
    /// </c>
    /// </code>
    /// </example>
    static member from(str: string) : Result<Row list, string> =
        let line = str.Split("\n")

        let digit = line[0] |> int

        seq {
            for row in line |> Seq.tail do
                if row |> String.startsWith "//" || row = "" then
                    match row.Split(" ") |> Array.toList with
                    | [ e ] -> yield Error($"Invalid syntax. \"{e}\"")
                    | [ labelName; num; "*" ] ->
                        let data = num |> Cell.from digit
                        let label = [ labelName, true ]

                        yield
                            { data = data
                              label = label
                              isEnd = false }
                            |> Ok
                    | [ labelName; num ] ->
                        let data = num |> Cell.from digit
                        let label = [ labelName, false ]

                        yield
                            { data = data
                              label = label
                              isEnd = false }
                            |> Ok
                    | _ -> ()
        }
        |> Seq.toList
        |> sequence
