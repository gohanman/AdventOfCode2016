open System.Text.RegularExpressions

type Node = { x:int; y:int; used:int; total:int }
type World = { nodes:Node list; goal: int*int }

let REGEX = new Regex("/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T")

let intify a = System.Int32.Parse(a)

let strToNode (s:string) =
    let m = REGEX.Match(s)
    match m.Success with
    | false -> None
    | true -> 
        let x = intify (m.Groups.Item(1).Value)
        let y = intify (m.Groups.Item(2).Value)
        let t = intify (m.Groups.Item(3).Value)
        let u = intify (m.Groups.Item(4).Value)
        Some { x=x; y=y; used=u; total=t }

let fits (a:Node) (b:Node) = 
    (a.used <> 0) && (a.used <= (b.total - b.used))

let rec viablePairs (nodes:Node list) (pairs) =
    if (nodes.Length <= 1) then pairs
    else
        let a = nodes.Head 
        let pairs' =
            nodes.Tail
            |> List.filter (fun i -> (fits a i) || (fits i a)) 
            |> List.map (fun i -> if (fits a i) then (a, i) else (i, a))
        viablePairs nodes.Tail (pairs @ pairs')

let adjacent (a:Node) (b:Node) =
    (a.y = b.y && abs (a.x - b.x) = 1) || (a.x = b.x && abs (a.y - b.y) = 1)

let isGoal (n:Node) (g:int*int) = (n.x, n.y) = g

let moveData (w:World) (src:Node) (dst:Node) =
    let dst' = { dst with used=(dst.used + src.used) }
    let src' = { src with used=0 }
    let nodes' = 
        w.nodes
        |> List.map (fun i ->
            if (src = i) then src'
            elif (dst = i) then dst'
            else i
        )
    let goal' =
        if (isGoal src w.goal) then (dst'.x, dst'.y)
        else w.goal
    { nodes=nodes'; goal=goal' }

let safeMove (a:Node) (b:Node) (g:int*int) =
    if ((isGoal a g) && b.used > 0) then false
    elif (isGoal b g) then false
    else true

let moves (world:World) = 
    viablePairs world.nodes []
    |> List.filter (fun i -> adjacent (fst i) (snd i))
    |> List.filter (fun i -> safeMove (fst i) (snd i) world.goal)

let solved (w:World list) =
    List.filter (fun i -> i.goal = (0, 0)) w
    |> List.isEmpty |> not

let goalCanMove (w:World list) =
    w
    |> List.filter (fun i ->
        let p = moves i
        let p' = List.filter (fun j -> isGoal (fst j) i.goal) p
        p'.Length > 0
    ) |> List.isEmpty |> not

let rec search (w:World list) (states:Set<World>) (steps:int) (solver) =
    printfn "%d %d" (w.Length) steps
    if (solver w) then (steps, w)
    else
        let mutable states' = states
        let mutable newWorlds = []
        for world in w do
            states' <- states'.Add(world)
            let pairs = moves world
            for p in pairs do
                let w' = moveData world (fst p) (snd p)
                if (not (states'.Contains(w'))) then
                    newWorlds <- w' :: newWorlds
        if (newWorlds.Length = 0) then
            printfn "Stuck at %d" steps
            (-1, [])
        else
            search newWorlds states' (steps+1) solver

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt")
    let nodes = 
        file |> Array.map strToNode |> Array.choose id |> Array.toList
    let pairs = viablePairs nodes []
    printfn "%d" pairs.Length
    let goal = nodes |> List.maxBy (fun i -> i.x)
    let world = { nodes=nodes; goal=(goal.x, 0) }
    let steps,worlds = search [world] Set.empty 0 goalCanMove
    printfn "Steps Phase 1: %d" steps
    let trimmed = 
        worlds
        |> List.filter (fun i ->
            let p = moves i
            let p' = List.filter (fun j -> isGoal (fst j) i.goal) p
            p'.Length > 0
        )
    let steps',worlds' = search trimmed Set.empty steps solved
    printfn "Steps Total: %d" steps'
    0 // return an integer exit code
