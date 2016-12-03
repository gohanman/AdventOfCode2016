
type Triangle = { sideA:int; sideB: int; sideC:int }

let valid t = 
    (t.sideA+t.sideB > t.sideC) && (t.sideA+t.sideC > t.sideB) && (t.sideB+t.sideC > t.sideA)

let matcher = new System.Text.RegularExpressions.Regex("\s*([0-9]+)\s+([0-9]+)\s+([0-9]+)\s*")

let tryTriangle (s:string) =
    let m = matcher.Match(s)
    let toInt (s:string) = System.Int32.Parse(s)
    match m.Success with
    | false -> None
    | true -> (
                let g = m.Groups
                let a = toInt (g.Item(1).Value)
                let b = toInt (g.Item(2).Value)
                let c = toInt (g.Item(3).Value)
                Some { sideA=a; sideB=b; sideC=c }
    )

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let triangles = 
        file
        |> List.map tryTriangle
        |> List.choose id
        |> List.filter valid
    printfn "Triangles: %d" triangles.Length
    0 // return an integer exit code
