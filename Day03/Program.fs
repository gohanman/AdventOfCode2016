
type Triangle = { sideA:int; sideB: int; sideC:int }

let valid t = 
    (t.sideA+t.sideB > t.sideC) && (t.sideA+t.sideC > t.sideB) && (t.sideB+t.sideC > t.sideA)

let matcher = new System.Text.RegularExpressions.Regex("\s*([0-9]+)\s+([0-9]+)\s+([0-9]+)\s*")
let toInt (s:string) = System.Int32.Parse(s)

let tryTriangle (s:string) =
    let m = matcher.Match(s)
    match m.Success with
    | false -> None
    | true -> (
                let g = m.Groups
                let a = toInt (g.Item(1).Value)
                let b = toInt (g.Item(2).Value)
                let c = toInt (g.Item(3).Value)
                Some { sideA=a; sideB=b; sideC=c }
    )

let tryTrio (s:string list) =
    let regex = List.map (fun i -> matcher.Match(i)) s
    seq {
        for i in 1..3 do
            let a = toInt (regex.Item(0).Groups.Item(i).Value)
            let b = toInt (regex.Item(1).Groups.Item(i).Value)
            let c = toInt (regex.Item(2).Groups.Item(i).Value)
            yield { sideA=a; sideB=b; sideC=c }
    }

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let triangles = 
        file
        |> List.map tryTriangle
        |> List.choose id
        |> List.filter valid
    printfn "Triangles: %d" triangles.Length

    let trios = List.chunkBySize 3 file
    let verts =
        trios
        |> List.map tryTrio
        |> List.map Seq.toList
        |> List.fold (fun c i -> c @ i) []
        |> List.filter valid
    printfn "Vertical Triangles: %d" verts.Length
    0 // return an integer exit code
