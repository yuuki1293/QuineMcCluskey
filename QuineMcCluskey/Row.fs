module Row

open System
open Cell

type Row =
    { data: Cell list; label: int list; isEnd: bool }
    static member from raw =
        raw
        |> Seq.map (function
            | '0' -> False
            | '1' -> True
            | '-' -> Both
            | e -> raise (Exception($"Unexpected char '{e}'")))
        |> Seq.toList
    
    override this.ToString() =
        let data =
            this.data
            |> Seq.map (fun i -> i.ToString())
            |> String.concat ""
        
        $"{data}" + " " + $"%A{this.label}"