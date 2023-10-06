module Debug

open Main
open Row

let wiki =
    lazy(calc ([4,false;8,false;9,true;10,false;11,false;12,false;14,true;15,false] |> Seq.map (genRow2 6)))

let resultA1 =
      lazy( calc ([ 51..63 ]
            |> Seq.map (genRow 6)))

let resultA2 =
      lazy( calc ([ 25..50 ]
            |> Seq.map (genRow 6)))

let printResult(result: Row seq) =
    result
    |> Seq.map (fun i -> i.ToString())
    |> Seq.iter (fun i -> printfn $"%s{i}")
    
printResult (wiki.Force())
