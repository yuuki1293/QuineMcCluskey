open System

type Cell =
    | False
    | True
    | Both
    override this.ToString() =
        match this with
        | False -> "0"
        | True -> "1"
        | Both -> "-"
        

type Row = { data: Cell list; label: int list; isEnd: bool }

let toRow(raw: String) =
    raw
    |> Seq.map (function
        | '0' -> False
        | '1' -> True
        | '-' -> Both
        | e -> raise (Exception($"Unexpected char '{e}'")))
    |> Seq.toList

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

let rowToString (row: Row) =
    let data =
        row.data
        |> Seq.map (fun i -> i.ToString())
        |> String.concat ""
        
    $"{data} {row.label}"

let summarize (source1: Row) (source2: Row) =
    let zipped =
        (source1.data, source2.data)
        ||> Seq.zip
    
    if zipped |> Seq.filter isBoth |> Seq.length <> 1 then
        raise (Exception($"source1 and source2 cannot be combined.\n\tsource1 is \"{rowToString source1}\"\n\tsource2 is \"{rowToString source2}\""))
    else
        let cells = (seq {
            for x, y in zipped do
                if isBoth (x, y) then
                    Cell.Both
                else
                    x
        } |> Seq.toList)
        
        let newTerm = source1.label |> Seq.append source2.label |> Seq.sort |> Seq.toList
        
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
                for flag, (hamming_j, comparison) in zipped do
                    if abs (hamming_j - hamming_i) = 1 then
                        if flag then
                            yield summarize row comparison 
            else
                yield {row with isEnd = true }
    }
    |> Seq.distinctBy (fun x -> x.data)

let rec calc (table: Row seq) =
    let bitTable = seq {
        for i in table do
            countTrue i, i
    }
    
    let hammingTable =
        bitTable
        |> Seq.sortBy fst

    let next = nextStep hammingTable
    
    if next |> Seq.forall (fun i -> i.isEnd) then
        next
    else
        calc next
    
    
let result =
      calc [{data = toRow "0100"; label= [4]; isEnd = false }
            {data = toRow "1000"; label= [8]; isEnd = false }
            {data = toRow "1001"; label= [9]; isEnd = false }
            {data = toRow "1010"; label= [10]; isEnd = false }
            {data = toRow "1011"; label= [11]; isEnd = false }
            {data = toRow "1100"; label= [12]; isEnd = false }
            {data = toRow "1110"; label= [14]; isEnd = false }
            {data = toRow "1111"; label= [15]; isEnd = false }]
      
result
|> Seq.map rowToString
|> Seq.iter (fun i -> printfn $"%s{i}")