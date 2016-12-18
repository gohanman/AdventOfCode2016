
type Tile =
    | Trap=0
    | Safe=1

type Triplet = { left:Tile; center:Tile; right:Tile }

let toTile (c:char) = if (c='.') then Tile.Safe else Tile.Trap
let fromTile (t:Tile) = if (t=Tile.Safe) then "." else "^"

let next (t:Triplet) =
    match t with
    | { left=Tile.Trap; center=Tile.Trap; right=Tile.Safe } -> Tile.Trap
    | { left=Tile.Safe; center=Tile.Trap; right=Tile.Trap } -> Tile.Trap
    | { left=Tile.Trap; center=Tile.Safe; right=Tile.Safe } -> Tile.Trap
    | { left=Tile.Safe; center=Tile.Safe; right=Tile.Trap } -> Tile.Trap
    | _ -> Tile.Safe

let toTriplet (s:string) (i:int) =
    let l = if (i=0) then Tile.Safe else toTile (s.Chars(i-1))
    let c = toTile (s.Chars(i))
    let r = if (i >= (s.Length - 1)) then Tile.Safe else toTile (s.Chars(i + 1))
    { left=l; center=c; right=r }

let nextLine (line:string) =
    [for i in 0..(line.Length - 1) -> toTriplet line i]
    |> List.map (fun i -> fromTile (next i))
    |> List.fold (fun c i -> c + i) ""

let rec getLines (line:string) (count:int) =
    seq {
        yield line
        if (count > 1) then
            let line' = nextLine line
            yield! getLines line' (count - 1)
    }

let countSafe (lines:string seq) =
    lines
    |> Seq.map (fun i -> i.Replace("^", ""))
    |> Seq.fold (fun c i -> c + i.Length) 0

[<EntryPoint>]
let main argv = 
    let input = "......^.^^.....^^^^^^^^^...^.^..^^.^^^..^.^..^.^^^.^^^^..^^.^.^.....^^^^^..^..^^^..^^.^.^..^^..^^^.."
    let lines = getLines input 40
    lines
    |> Seq.iter (fun i -> printfn "%s" i)
    let safeCount = countSafe lines
    printfn "Safe %d" safeCount
    let lines' = getLines input 400000
    let safeCount' = countSafe lines'
    printfn "Safe %d" safeCount'
    0 // return an integer exit code
