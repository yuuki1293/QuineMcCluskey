﻿module Row

open System
open Cell

type Label = (string * bool) list

type Row =
    { data: Cell list; label: Label; isEnd: bool }
    
    override this.ToString() =
        let data =
            this.data
            |> Seq.map (fun i -> i.ToString())
            |> String.concat ""
            
        let label =
            this.label
            |> List.map fst
        
        sprintf $"{data} %A{label}"
        
    static member or_ (l, r: Row) =
        l @ r.label
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toList