
type Heading =
    | North
    | East
    | South
    | West 

type Location = { x: int; y:int; facing:Heading }
type Direction = Left | Right
type Move = { turn:Direction; step:int }

let strToMove (s:string) =
    let first = s.Substring(0, 1)
    let rest = s.Substring(1)
    let dir = 
        match first with
        | "L" -> Left
        | "R" -> Right
        | _ -> failwith "Invalid input string"
    { turn=dir; step=System.Int32.Parse(rest) }

let PATH = "L4, L1, R4, R1, R1, L3, R5, L5, L2, L3, R2, R1, L4, R5, R4, L2, R1, R3, L5, R1, L3, L2, R5, L4, L5, R1, R2, L1, R5, L3, R2, R2, L1, R5, R2, L1, L1, R2, L1, R1, L2, L2, R4, R3, R2, L3, L188, L3, R2, R54, R1, R1, L2, L4, L3, L2, R3, L1, L1, R3, R5, L1, R5, L1, L1, R2, R4, R4, L5, L4, L1, R2, R4, R5, L2, L3, R5, L5, R1, R5, L2, R4, L2, L1, R4, R3, R4, L4, R3, L4, R78, R2, L3, R188, R2, R3, L2, R2, R3, R1, R5, R1, L1, L1, R4, R2, R1, R5, L1, R4, L4, R2, R5, L2, L5, R4, L3, L2, R1, R1, L5, L4, R1, L5, L1, L5, L1, L4, L3, L5, R4, R5, R2, L5, R5, R5, R4, R2, L1, L2, R3, R5, R5, R5, L2, L1, R4, R3, R1, L4, L2, L3, R2, L3, L5, L2, L2, L1, L2, R5, L2, L2, L3, L1, R1, L4, R2, L4, R3, R5, R3, R4, R1, R5, L3, L5, L5, L3, L2, L1, R3, L4, R3, R2, L1, R3, R1, L2, R4, L3, L3, L3, L1, L2"
let turnList (path:string) = 
    path.Replace(" ", "").Split([|','|]) |> Array.toList

let moveList (s:string) =
    turnList s |> List.map strToMove

let turnLeft h =
    match h with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnRight h =
    match h with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let walk (loc:Location) (m:Move) =
    let newHeading =
        match m.turn with
        | Left -> turnLeft loc.facing
        | Right -> turnRight loc.facing
    let newX, newY = 
        match newHeading with
        | North -> (loc.x, loc.y + m.step)
        | East -> (loc.x + m.step, loc.y)
        | South -> (loc.x, loc.y - m.step)
        | West -> (loc.x - m.step, loc.y)
    { x=newX; y=newY; facing=newHeading }

let pointsBetween (a:Location) (b:Location) =
    if (a.x = b.x && a.y < b.y) then
        [for i in (a.y+1)..(b.y) -> (a.x, a.y+i)]
    elif (a.x = b.x && a.y > b.y) then
        [for i in (b.y+1)..(a.y) -> (b.x, b.y+i)]
    elif (a.y = b.y && a.x < b.x) then
        [for i in (a.x+1)..(b.x) -> (a.x+i, a.y)]
    elif (a.y = b.y && a.x > b.x) then
        [for i in (b.x+1)..(a.x) -> (b.x+i, b.y)]
    else
        []

let findDupe (loc:Location) (m:Move list) =
    let mutable points = Map.empty.Add((loc.x,loc.y), 1)
    let mutable current = loc
    seq {
        for i in m do
            let newLoc = walk current i
            for j in (pointsBetween current newLoc) do
                let x,y = j
                if (points.ContainsKey((x, y))) then 
                    yield (x, y)
                let newPoints = points.Add((x,y), 1)
                points <- newPoints
            current <- newLoc
    }

[<EntryPoint>]
let main argv = 
    let moves = moveList PATH
    let start = { x=0; y=0; facing=North }
    let finish = List.fold walk start moves
    printfn "End at: %d,%d" finish.x finish.y
    printfn "Blocks away: %d" (finish.x + finish.y)
    let dupes = findDupe start moves
    let first = Seq.take 1 dupes |> Seq.toList
    let x,y = first.Head
    printfn "End at: %d,%d" x y
    printfn "Blocks away: %d" (x + y)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
