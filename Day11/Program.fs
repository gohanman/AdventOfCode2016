
type Element =
    | Strontium = 38
    | Ruthenium = 44
    | Thulium = 69
    | Plutonium = 94
    | Curium = 96

type Part =
    | Generator of Element
    | Microchip of Element

type Floor = { parts:Part list }
type World = { floors:Floor array; elevator:int }
type Move = { parts:Part list; source:int; dest:int; }

let safeOnFloor (p:Part) (f:Floor) =
    match p with
    | Generator g ->
        let g' = int g
        f.parts
        |> List.filter (fun i ->
            match i with
            | Microchip m -> (int m)=g'
            | _ -> false
        )
        |> List.isEmpty |> not
    | Microchip m ->
        let m' = int m
        f.parts
        |> List.filter (fun i ->
            match i with
            | Generator g -> (int g)=m'
            | _ -> false
        )
        |> List.isEmpty |> not

let rankWorld (w:World) =
    w.floors
    |> Array.mapi (fun i x -> i * x.parts.Length)
    |> Array.fold (fun c i -> c + i) 0

let addParts (w:World) (floor:int) (p:Part list) =
    let a =
        w.floors
        |> Array.mapi (fun i x ->
            if (i=floor) then { x with parts=(p @ x.parts) }
            else x
        )
    { w with floors=a }

let partEquals (a:Part) (b:Part) =
    match a with
    | Generator g ->
        match b with
        | Generator g' -> (int g)=(int g')
        | _ ->  false
    | Microchip m ->
        match b with
        | Microchip m' -> (int m)=(int m')
        | _ -> false

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

let floorMatches (a:Floor) (b:Floor) =
    let f =
        a.parts
        |> List.map (fun i -> 
            List.exists (fun j -> partEquals i j) b.parts
        )
        |> List.fold (fun c i -> c && i) true
    (f && a.parts.Length=b.parts.Length)

let worldMatches (w:World) (x:World) =
    let f =
        w.floors
        |> Array.mapi (fun i j ->
            floorMatches j (x.floors.[i])
        )
        |> Array.fold (fun c i -> c && i) true
    (f && w.elevator=x.elevator)

let worldChecked (wl:World seq) (w:World) =
    Seq.contains w wl
    //Seq.exists (fun i -> worldMatches i w) wl
    
let rec removeParts (w:World) (floor:int) (p:Part list) =
    match p.Length with
    | 0 -> w
    | _ ->
        let cur = p.Head
        let a = 
            w.floors
            |> Array.mapi (fun i x ->
                if (i=floor) then { Floor.parts=(List.filter (fun j -> not(partEquals cur j)) x.parts) }
                else x
            )
        let w' = { w with floors=a }
        removeParts w' floor p.Tail

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

let rec moves (w:World) =
    seq {
        let f = w.floors.[w.elevator]
        let source=w.elevator
        if (canMoveUp w) then
            let dest = w.elevator+1
            let up = w.floors.[dest]
            for i in f.parts do
                if (safeOnFloor i up) then 
                    yield { parts=[i]; source=source; dest=dest }
                for j in f.parts.Tail do
                    if ((safeOnFloor i up) && (safeOnFloor j up)) then
                        yield { parts=[i;j]; source=source; dest=dest }
                    elif (partMatches i j) then
                        yield { parts=[i;j]; source=source; dest=dest }
        if (canMoveDown w) then
            let dest = w.elevator-1
            let down = w.floors.[dest]
            for i in f.parts do
                if (safeOnFloor i down) then 
                    yield { parts=[i]; source=source; dest=dest }
                for j in f.parts.Tail do
                    if ((safeOnFloor i down) && (safeOnFloor j down)) then
                        yield { parts=[i;j]; source=source; dest=dest }
                    elif (partMatches i j) then
                        yield { parts=[i;j]; source=source; dest=dest }
        if (f.parts.Length > 1) then
            let f' =
                w.floors            
                |> Array.mapi (fun i x ->
                    if (i=source) then { Floor.parts=f.parts.Tail }
                    else x
                )
            yield! moves { w with floors=f' }
    }

let makeMove (w:World) (m:Move) =
    let w' = removeParts w m.source m.parts
    let w'' = addParts w' m.dest m.parts
    let dir = if m.source < m.dest then 1 else -1
    { w'' with elevator=(w.elevator + dir) }

let rec solve (w:World seq) (num:int) (visited:World seq) =
    if (Seq.exists (fun i -> solved i) w) then
        printfn "Solved in %d" num
    elif (num < 1024) then
        let visits = Seq.append visited w
        let w' =
            w
            |> Seq.map (fun i -> 
                let m = moves i
                Seq.map (fun j -> makeMove i j) m
            )
            |> Seq.concat
            |> Seq.filter (fun i -> not (worldChecked visits i))
        printfn "%d" num
        w'
        |> Seq.iter (fun i ->
            solve [i] (num+1) (Seq.append w' visits)
        )

[<EntryPoint>]
let main argv = 
    let floors = [|
        { Floor.parts=[ 
                        Generator Element.Strontium; 
                        Microchip Element.Strontium;
                        Generator Element.Plutonium; 
                        Microchip Element.Plutonium;
        ] };
        { parts=[ 
                Generator Element.Thulium;
                Generator Element.Ruthenium;
                Microchip Element.Ruthenium;
                Generator Element.Curium;
                Microchip Element.Curium;
        ] };
        { parts=[ Microchip Element.Thulium] };
        { parts=[] };
    |]
    let world = { floors=floors; elevator=0 }
    solve [world] 0 []
    0 // return an integer exit code
