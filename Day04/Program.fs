
type Room = { name:string; sector:int; checksum:string }
type Letter = { value:char; frequency:int }

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

let sortLetters a b =
    if (a.frequency > b.frequency) then
        1
    else if (a.frequency < b.frequency) then
        -1
    else
        -1 * (a.value.CompareTo(b.value))

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
    dict.Keys
    |> Seq.map (fun i -> { value=i; frequency=dict.[i] } )
    |> Seq.toList
    |> List.sortWith sortLetters
    |> List.rev

let listContains (l:Letter list) (i:char) =
    let check = List.filter (fun a -> a.value = i) l
    match check.Length with
    | 0 -> false
    | _ -> true

let valid (r:Room) =
    let chars = countChars (r.name)
    let allowed = Seq.toList (Seq.take 5 chars)
    let proj = 
        r.checksum
        |> Seq.filter (fun i -> i <> '-')
        |> Seq.filter (fun i -> not(listContains allowed i))
        |> Seq.toList
    match proj.Length with
    | 0 -> true
    | _ -> false

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let rooms = List.map strToRoom file |> List.choose id
    let goodRooms = List.filter valid rooms
    let sum = List.fold (fun c i -> c+i.sector) 0 goodRooms
    printfn "Room count: %d, Sector sum: %d" (goodRooms.Length) sum
    0 // return an integer exit code
