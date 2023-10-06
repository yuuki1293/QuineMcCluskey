module Main

open System
open Cell
open Row

let private countTrue (row: Row) =
    row.data
    |> Seq.filter (fun i -> i = True)
    |> Seq.length

let private isBoth (source1: Cell, source2: Cell) =
    if source1 = Both || source2 = Both then
        false
    else source1 <> source2

let private canSummarize (source1: Row) (source2: Row) =
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

let private summarize (source1: Row) (source2: Row) =
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

let private nextStep(hammingTable: (int*Row) seq) =
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

let genRow2 (digit: int) (num: int, dontCare: bool) =
    let data = Convert.ToString(num, 2).PadLeft(digit, '0') |> Row.from
    {data = data; label = [string num, dontCare]; isEnd = false}

let genRow (digit: int) (num: int) = genRow2 digit (num, false)

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

let private getMinTerms (table: Row seq) =
    table
    |> Seq.map (fun i -> i.label)
    |> Seq.concat
    |> Seq.filter (fun (_, dontCare) -> not dontCare)
    |> Seq.distinct
    |> Seq.toList

let private primeImplicant (minTerms: Label) (table: Row seq) =
    seq {
        for i in minTerms do
            let satisfyTerm =
                table
                |> Seq.filter (fun cell -> cell.label |> Seq.contains i)
            
            if satisfyTerm |> Seq.length = 1 then
                yield satisfyTerm |> Seq.head
    }
    |> Seq.distinct
    |> Seq.toList
    
let private remainingTerm (minTerms: Label) (table: Row seq) =
    table
    |> Seq.map (fun row -> row.label)
    |> Seq.fold (fun terms label -> terms |> Seq.except label) minTerms
    |> Seq.sort
    |> Seq.toList

let rec private comb: int * 'a list -> 'a list list =
    function
    | 0, _ -> [[]]
    | _, [] -> []
    | n, x::xs ->
        seq {
            for y in comb (n-1, xs) do
                yield x::y
        } |> Seq.toList |> (@) (comb (n, xs))

let private findSimplest (terms: Label) (implicants: Row list) =
    if terms.Length = 0 then
        []
    else
        seq {
            for termCount in [1..terms |> Seq.length] do
                let combs = comb (termCount, implicants)
                let simplest =
                    combs
                    |> List.tryFind (fun rows ->
                        let sum = Seq.fold (fun acc row -> Row.or_ (acc, row)) [] rows
                        terms |> List.forall (fun term -> sum |> List.contains term))
                    
                match simplest with
                | None -> ()
                | Some value -> yield value
        }
        |> Seq.head
        |> Seq.toList

let calcSimplest (table: Row seq) =
    let minTerms = getMinTerms table
    let primeImplicants = primeImplicant minTerms table
    
    let otherImplicants =
        table
        |> Seq.except primeImplicants
        |> Seq.toList
        
    let remainingTerms =
        remainingTerm minTerms primeImplicants
    
    let simplestRemainingTerms =
        findSimplest remainingTerms otherImplicants
    
    primeImplicants @ simplestRemainingTerms
    |> List.sort

let calc (table: Row seq) =
    calcMainTerm table
    |> calcSimplest