
type Element =
    | Strontium = 38
    | Ruthenium = 44
    | Thulium = 69
    | Plutonium = 94
    | Curium = 96
    | Elerium = 100
    | Dilithium = 101

type Part =
    | Generator of Element
    | Microchip of Element

type Floor = { parts:Set<Part> }
type World = { floors:Floor array; elevator:int }
type Move = { parts:Set<Part>; source:int; dest:int; }
type MetaFloor = { chips:int; generators:int; pairs:int }
type MetaWorld = { floors:MetaFloor array; elevator:int }

let isGenerator (p:Part) =
    match p with
    | Generator g -> true
    | _ -> false

let safeOnFloor (p:Part) (f:Floor) =
    match p with
    | Generator g -> true
    | Microchip m ->
        let m' = int m
        let generators = f.parts |> Seq.filter isGenerator |> Seq.toList
        if (generators.Length=0) then true
        else
            f.parts
            |> Set.filter (fun i ->
                match i with
                | Generator g -> (int g)=m'
                | _ -> false
            )
            |> Set.isEmpty |> not

let rankWorld (w:World) =
    w.floors
    |> Array.mapi (fun i x -> i * x.parts.Count)
    |> Array.fold (fun c i -> c + i) 0

let addParts (w:World) (floor:int) (p:Set<Part>) =
    let a = Array.copy w.floors
    a.[floor] <- { parts=(Set.union p w.floors.[floor].parts) }
    { w with floors=a }

let partMatches (a:Part) (b:Part) =
    match a with
    | Generator g ->
        match b with
        | Microchip m -> (int g)=(int m)
        | _ -> false
    | Microchip m ->
        match b with
        | Generator g -> (int m)=(int g)
        | Microchip m -> false

let metaFloor (f:Floor) =
    let g = f.parts |> Set.filter isGenerator |> Set.count
    let m = f.parts.Count - g
    let arr = Set.toArray f.parts
    let mutable p = 0
    for i in [0..(arr.Length - 1)] do
        for j in [(i+1)..(arr.Length - 1)] do
            if (partMatches arr.[i] arr.[j]) then p <- p + 1
    { chips=m; generators=g; pairs=p; }

let metaState (w:World) =
    let f = Array.copy w.floors
    let mf = Array.map (fun i -> metaFloor i) f
    { MetaWorld.floors=mf; elevator=w.elevator }

let worldChecked (wl:World seq) (w:World) =
    Seq.contains w wl
    
let removeParts (w:World) (floor:int) (p:Set<Part>) =
    let keep,_ = 
        w.floors.[floor].parts 
        |> Set.partition (fun i -> not (p.Contains(i)))
    let f = Array.copy w.floors
    f.[floor] <- { parts=keep }
    { w with floors=f }

let solved (w:World) =
    let top = w.floors.Length - 1
    w.floors
    |> Array.mapi (fun i x -> 
        if (i=top) then not x.parts.IsEmpty
        else x.parts.IsEmpty
    )
    |> Array.fold (fun c i -> c && i) true

let canMoveUp (w:World) = w.elevator < (w.floors.Length - 1)
let canMoveDown (w:World) = w.elevator > 0

let moves (w:World) =
    seq {
        let f = w.floors.[w.elevator]
        let pl = Set.toArray f.parts
        let source=w.elevator
        if (canMoveUp w) then
            let dest = w.elevator+1
            let up = w.floors.[dest]
            for index in [0..(pl.Length - 1)] do
                let i = pl.[index]
                if (safeOnFloor i up) then 
                    yield { parts=Set.empty.Add(i); source=source; dest=dest }
                for jndex in [(index+1)..(pl.Length - 1)] do
                    let j = pl.[jndex]
                    if ((safeOnFloor i up) && (safeOnFloor j up)) then
                        yield { parts=Set.empty.Add(i).Add(j); source=source; dest=dest }
                    elif (partMatches i j) then
                        yield { parts=Set.empty.Add(i).Add(j); source=source; dest=dest }
        if (canMoveDown w) then
            let dest = w.elevator-1
            let down = w.floors.[dest]
            for index in [0..(pl.Length - 1)] do
                let i = pl.[index]
                if (safeOnFloor i down) then 
                    yield { parts=Set.empty.Add(i); source=source; dest=dest }
                else
                    for jndex in [(index+1)..(pl.Length - 1)] do
                        let j = pl.[jndex]
                        if ((safeOnFloor i down) && (safeOnFloor j down)) then
                            yield { parts=Set.empty.Add(i).Add(j); source=source; dest=dest }
                        elif (partMatches i j) then
                            yield { parts=Set.empty.Add(i).Add(j); source=source; dest=dest }
    }

let itemCount (w:World) =
    w.floors |> Array.fold (fun c i -> c + i.parts.Count) 0

let makeMove (w:World) (m:Move) =
    let w' = removeParts w m.source m.parts
    let w'' = addParts w' m.dest m.parts
    if ((itemCount w')=(itemCount w'')) then 
        printfn ""
        printfn "MOVE %A" m
        printfn "PRE %A" w'
        printfn "POST %A" w''
        printfn ""
    { w'' with elevator=m.dest }

let floorCount (w:World) (n:int) =
    w.floors.[n].parts.Count

let solve (w:World) =
    let mutable searching = true
    let mutable count = 0
    let mutable visited = Set.empty.Add(metaState w)
    let mutable queue = Set.empty.Add(w)
    while (searching) do
        printfn "%d, %d" count visited.Count
        for world in queue do
            if (solved world) then  
                printfn "Solved in %d" count
                searching <- false
            else
                let m = moves world
                let worlds = 
                    m 
                    |> Seq.map (fun i -> makeMove world i) 
                    |> Seq.filter (fun i -> not (visited.Contains(metaState i)))
                    |> Seq.sortBy rankWorld
                    |> Seq.toList
                for newWorld in worlds do
                    queue <- queue.Add(newWorld)
                    visited <- visited.Add(metaState newWorld)
        count <- count + 1

[<EntryPoint>]
let main argv = 
    let floors = [|
        { Floor.parts=Set.empty
            .Add(Generator Element.Strontium) 
            .Add(Microchip Element.Strontium)
            .Add(Generator Element.Plutonium)
            .Add(Microchip Element.Plutonium)
        };
        { parts=Set.empty
            .Add(Generator Element.Thulium)
            .Add(Generator Element.Ruthenium)
            .Add(Microchip Element.Ruthenium)
            .Add(Generator Element.Curium)
            .Add(Microchip Element.Curium)
        };
        { parts=Set.empty.Add(Microchip Element.Thulium) };
        { parts=Set.empty};
    |]
    let world = { World.floors=floors; elevator=0 }
    //solve world
    let f0 = world.floors.[0].parts
    let additions = 
        f0.Add(Generator Element.Elerium).Add(Microchip Element.Elerium).Add(Generator Element.Dilithium).Add(Microchip Element.Dilithium)
    floors.[0] <- { parts=additions }
    let world' = { World.floors=floors; elevator=0 }
    solve world'
    0 // return an integer exit code
