
type Room = { name:string; sector:int; checksum:string }

let matcher = new System.Text.RegularExpressions.Regex("(.+)-([0-9]+)\[(.+)\]")

let strToRoom (s:string) =
    let m = matcher.Match(s)
    match m.Success with
    | false -> None
    | true -> (
                let g = m.Groups
                let rName = g.Item(1).Value
                let rSector = System.Int32.Parse(g.Item(2).Value)
                let rCheck = g.Item(3).Value
                let r:Room = { name=rName; sector=rSector; checksum=rCheck }
                Some r
    )

let countChars (s:string) =
    let mutable dict = new System.Collections.Generic.Dictionary<char, int>()
    [for c in s -> c] 
    |> List.filter (fun i -> i <> '-')
    |> List.iter (fun i ->
        if not(dict.ContainsKey(i)) then
            dict.Add(i, 1)
        else
            dict.[i] <- dict.[i] + 1
        ()
    )
    dict

let valid (r:Room) =
    let chars = countChars (r.name)
    ()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
