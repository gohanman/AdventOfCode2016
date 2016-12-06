
let linesToChars (lines:string list) =
    lines |> List.map (fun i -> [for c in i -> c])

let nthSet (n:int) (data:char list list) =
    data |> List.map (fun i -> i.Item n)

let newMap (c:char) (map:Map<char,int>) =
    let cur = if (map.ContainsKey(c)) then map.Item(c) else 0    
    map.Add(c, cur+1)

let rec frequency (data:char list) (map:Map<char,int>) =
    match data with
    | [] -> map
    | x::xs -> 
        let map' = newMap x map
        frequency xs map'

let rec mapMax (map:Map<char,int>) (curMax:int) (curVal:char) =
    match (map.Count) with
    | 0 -> curMax, curVal
    | _ ->
        let newKey = map |> Map.toSeq |> Seq.map fst |> Seq.head
        let newMax = map.Item(newKey)
        let trimMap = map.Remove(newKey)
        if (curMax < newMax) then
            mapMax trimMap newMax newKey
        else
            mapMax trimMap curMax curVal

let rec mapMin (map:Map<char,int>) (curMin:int) (curVal:char) =
    match (map.Count) with
    | 0 -> curMin, curVal
    | _ ->
        let newKey = map |> Map.toSeq |> Seq.map fst |> Seq.head
        let newMin = map.Item(newKey)
        let trimMap = map.Remove(newKey)
        if (curMin > newMin) then
            mapMin trimMap newMin newKey
        else
            mapMin trimMap curMin curVal

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let charlists = linesToChars file
    let headlist = charlists.Head
    for i in 0..(headlist.Length-1) do
        let curSet = nthSet i charlists
        let freq = frequency curSet Map.empty
        let _,letter = mapMax freq 0 '.'
        let _,minLetter = mapMin freq 999 '.'
        printfn "%d: %s" i (letter.ToString())
        printfn "\t%d: %s" i (minLetter.ToString())
    0 // return an integer exit code
