module Main

open System
open Cell
open Row

let countTrue (row: Row) =
    row.data
    |> Seq.filter (fun i -> i = True)
    |> Seq.length

let isBoth (source1: Cell, source2: Cell) =
    if source1 = Both || source2 = Both then
        false
    else source1 <> source2

let canSummarize (source1: Row) (source2: Row) =
    let isSameBoth =
        (source1.data, source2.data)
        ||> Seq.forall2
                (fun i j ->
                         match i, j with
                         | Both, Both -> true
                         | Both, _ -> false
                         | _, Both -> false
                         | _, _ -> true)
    
    let diffCount =
        (source1.data, source2.data)
        ||>Seq.zip
        |> Seq.where isBoth
        |> Seq.length
        
    isSameBoth && diffCount = 1

let summarize (source1: Row) (source2: Row) =
    let zipped =
        (source1.data, source2.data)
        ||> Seq.zip
    
    if zipped |> Seq.filter isBoth |> Seq.length <> 1 then
        raise (Exception($"source1 and source2 cannot be combined.\n\tsource1 is \"{source1}\"\n\tsource2 is \"{source2}\""))
    else
        let cells = (seq {
            for x, y in zipped do
                if isBoth (x, y) then
                    Both
                else
                    x
        } |> Seq.toList)
        
        let newTerm = source1.label @ source2.label |> Seq.sort |> Seq.toList
        
        {data = cells; label = newTerm; isEnd = false }

let nextStep(hammingTable: (int*Row) seq) =
    seq {
        for hamming_i, row in hammingTable do
            let summarizeList = (seq {
                for hamming_j, comparison in hammingTable do
                    if abs (hamming_j - hamming_i) = 1 then
                        yield canSummarize row comparison
                    else
                        yield false
            })
            
            if summarizeList |> Seq.exists id then
                let zipped = (summarizeList, hammingTable) ||> Seq.zip
                for flag, (_, comparison) in zipped do
                    if flag then
                        yield summarize row comparison
            else
                yield { row with isEnd = true }
    }
    |> Seq.distinctBy (fun x -> x.data)

let genRow (digit: int) (num: int) =
    let data = Convert.ToString(num, 2).PadLeft(digit, '0') |> Row.from
    {data = data; label = [num]; isEnd = false}

let rec calcMainTerm (table: Row seq) =
    let bitTable = seq {
        for i in table do
            countTrue i, i
    }
    
    let hammingTable =
        bitTable
        |> Seq.sortBy fst

    let next = nextStep hammingTable |> Seq.toList
    
    if next |> Seq.forall (fun i -> i.isEnd) then
        next
    else
        calcMainTerm next
