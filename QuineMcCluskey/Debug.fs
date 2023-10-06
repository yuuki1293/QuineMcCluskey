module Debug

open Main
open Row

let wiki =
    lazy(calc ([4,false;8,false;9,true;10,false;11,false;12,false;14,true;15,false] |> Seq.map (genRow2 6)))

let resultB1 =
      lazy( calc ([ 51..63 ]
            |> Seq.map (genRow 6)))

let resultC1 =
      lazy( calc ([ 25..50 ]
            |> Seq.map (genRow 6)))

let resultD1 =
    lazy(calc ([12..24]@[38..50] |> Seq.map (genRow 6)))

let resultA0 =
    lazy(calc ([10;11;23;24;35;36;37;48;49;50;61;62;63] |> Seq.map (genRow 6)))

let resultB0 =
    lazy(calc ([5..9]@[17..22]@[30..34]@[43..47]@[56..60] |> Seq.map (genRow 6)))

let resultC0 =
    lazy(calc ([2;3;4;7;8;9;15;16;20;21;22;28;29;33;34;41;42;46;47;53;54;55;58;59;60] |> Seq.map (genRow 6)))

let resultD0 =
    lazy(calc ([1;3;4;6;9;11;14;16;19;21;22;24;26;27;29;32;34;37;39;40;42;44;45;47;49;50;52;55;57;60;62;63] |> Seq.map (genRow 6)))

let printResult(result: Row seq) =
    result
    |> Seq.map (fun i -> i.ToString())
    |> Seq.iter (fun i -> printfn $"%s{i}")
    
printResult (resultD0.Force())
