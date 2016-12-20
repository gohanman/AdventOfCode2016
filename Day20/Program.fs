
type Range = { min:uint32; max:uint32 }

let strToRange (s:string) =
    let parts = s.Split([|'-'|], 2)
    { 
        min=(System.UInt32.Parse(parts.[0]));
        max=(System.UInt32.Parse(parts.[1]));
    }

let rec validOptions (blacklists:Range list) (next:uint32) =
    seq {
        if (blacklists.Length > 0) then
            let bl = blacklists.Head
            if (next < bl.min) then
                for i in seq {next..(bl.min - 1u)} do yield i
            let next' = bl.max + 1u
            yield! validOptions blacklists.Tail next'
    }

let rec countOptions (blacklists:Range list) (next:uint32) =
    seq {
        if (blacklists.Length > 1) then
            let bl = blacklists.Head
            printfn "%d %d %d" bl.min bl.max next
            if (next < bl.min) then
                printfn "Yield %d" (bl.min - next)
                yield (bl.min - next)
            if (bl.max < 4294967295u) then
                let next' = if (next > bl.max) then next else bl.max + 1u
                yield! countOptions blacklists.Tail next'
    }

let rec noSublists (ranges:Range list) (good:Range list) =
    if (ranges.Length = 0) then good
    else
        let r = ranges.Head 
        let filtered =
            ranges.Tail
            |> List.filter (fun i -> not (i.min > r.min && i.max < r.max))
        noSublists filtered (r :: good)

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt")
    let ranges =
        file
        |> Array.map strToRange
        |> Array.sortBy (fun i -> i.min)
        |> Array.toList
    let valid = validOptions ranges 0u
    printfn "%A" (Seq.take 1 valid)
    let all =
        countOptions ranges 0u
        |> Seq.fold (fun c i -> c + i) 0u
    printfn "%d" all
    0 // return an integer exit code
