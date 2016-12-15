
type Disc = { positions:int; current: int }
type World = { discs:Disc array; time:int; capsule:int }

let rotateDisc (d:Disc) =
    let pos = (d.current + 1) % d.positions
    { d with current=pos }

let capsuleFalling (w:World) =
    let index = w.capsule - 1
    match (index >= 0 && index < w.discs.Length) with
    | false -> true
    | true -> if (w.discs.[index].current=0) then true else false

let finished (w:World) = w.capsule > w.discs.Length

let tick (w:World) =
    let d' = w.discs |> Array.map (fun i -> rotateDisc i)
    { discs=d'; time=(w.time+1); capsule=(w.capsule+1) }

let rec tickN (w:World) (n:int) =
    if (n=0) then w
    else tickN (tick w) (n-1)

let rec safePassage (w:World) =
    if (finished w) then true
    elif (not (capsuleFalling w)) then false
    else safePassage (tick w)

let rec tryTimes (d:Disc array) (t:int) =
    let w = { discs=d; time=t; capsule=0 }
    let w' = { (tickN w t) with capsule=0 }
    if (safePassage w') then t
    else tryTimes d (t+1)

[<EntryPoint>]
let main argv = 
    let d = [|
        { positions=17; current=1 };
        { positions=7; current=0 };
        { positions=19; current=2 };
        { positions=5; current=0 };
        { positions=3; current=0 };
        { positions=13; current=5 };
    |]
    (*
    let d = [|
        { positions=5; current=4 };
        { positions=2; current=1 };
    |]
    *)
    printfn "%d" (tryTimes d 0)
    0 // return an integer exit code
