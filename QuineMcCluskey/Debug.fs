module Debug

open Main
open Row

let resultA1 =
      lazy( calcMainTerm ([ 51..63 ]
            |> Seq.map (genRow 6)))

let resultA2 =
      lazy( calcMainTerm ([ 25..50 ]
            |> Seq.map (genRow 6)))

let printResult(result: Row seq) =
    result
    |> Seq.map (fun i -> i.ToString())
    |> Seq.iter (fun i -> printfn $"%s{i}")
    
printResult (resultA2.Force())
