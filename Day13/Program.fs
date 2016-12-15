
type Coord = { x:int; y:int }

[<Literal>]
let FAVORITE_NUMBER = 1362

let isOpen (c:Coord) =
    let num = (c.x*c.x) + (3*c.x) + (2*c.x*c.y) + c.y + (c.y*c.y) + FAVORITE_NUMBER
    let bin = System.Convert.ToString(num, 2)
    let ones = 
        [for x in bin -> x]
        |> List.filter (fun i -> i='1')
    ((ones.Length % 2) = 0)

let moveX (c:Coord) (a:int) =
    let c' = { c with x=(c.x + a) } 
    if (c'.x < 0) then None
    elif (isOpen c') then Some c'
    else None

let moveY (c:Coord) (a:int) =
    let c' = { c with y=(c.y + a) } 
    if (c'.y < 0) then None
    elif (isOpen c') then Some c'
    else None

let moves (c:Coord) = 
    [(moveX c 1); (moveX c -1); (moveY c 1); (moveY c -1)]
    |> List.choose id

let printMaze (visited:Set<Coord>) =
    for x in 0..31 do
        for y in 0..39 do
            let v = isOpen ({x=x;y=y})
            let s =
                if (visited.Contains({x=x;y=y})) then "0" 
                elif v then "." 
                else "#"
            printf "%s" s
        printfn ""

let rec goToPoint (cur:Coord) (target:Coord) (steps:int) (visited:Set<Coord>) =
    seq {
        if (cur=target) then 
            yield steps
        else
            let next = 
                moves cur 
                |> List.filter (fun i -> not (visited.Contains(i)))
            if (next.Length=0) then
                yield 0
            else
                for n in next do
                    let v' = visited.Add(cur) 
                    yield! goToPoint n target (steps+1) v'
    } 

let shortestPath (cur:Coord) (target:Coord) =
    let paths = 
        goToPoint cur target 0 Set.empty 
        |> Seq.filter (fun i -> i<>0)
        |> Seq.toList
    if (paths.Length=0) then 0 else List.min paths

let rec fanout (cur:Coord) (limit:int) (visited:Set<Coord>) =
    if (limit=0) then visited.Add(cur)
    else
        let v' = visited.Add(cur)
        let next = 
            moves cur
            |> List.filter (fun i -> not (v'.Contains(i)))
        [for n in next -> fanout n (limit-1) v']
        |> List.fold (fun c i -> Set.union c i) v'

[<EntryPoint>]
let main argv = 
    printfn "%d" (shortestPath ({x=1;y=1}) ({x=31;y=39}))
    let allpoints = fanout ({x=1;y=1}) 50 Set.empty
    printfn "%d" allpoints.Count
    0 // return an integer exit code
