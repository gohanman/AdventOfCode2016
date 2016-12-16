
type Disc = { positions:int; current: int }
type World = { discs:Disc array; time:int; capsule:int }

let rotateDisc (d:Disc) (amt:int) =
    let pos = (d.current + amt) % d.positions
    { d with current=pos }

let capsuleFalling (w:World) =
    let index = w.capsule - 1
    match (index >= 0 && index < w.discs.Length) with
    | false -> true
    | true -> if (w.discs.[index].current=0) then true else false

let finished (w:World) = w.capsule > w.discs.Length

let tick (w:World) =
    let d' = w.discs |> Array.map (fun i -> rotateDisc i 1)
    { discs=d'; time=(w.time+1); capsule=(w.capsule+1) }

let tickN (w:World) (n:int) =
    let d' = w.discs |> Array.map (fun i -> rotateDisc i n)
    { w with discs=d' }

let rec safePassage (w:World) =
    if (finished w) then true
    elif (not (capsuleFalling w)) then 
        false
    else safePassage (tick w)

let aligned (w:World) =
    let slots =
        w.discs
        |> Array.mapi (fun i x ->
            let time = i+1
            let rotate = (x.current + time) % x.positions
            rotate
        )
        |> Array.filter (fun i -> i<>0)
    if (slots.Length=0) then true else false

let rec tryTimes (d:Disc array) (t:int) =
    if (t%10000 = 0) then printfn "%d" t
    let w = { discs=d; time=t; capsule=0 }
    let w' = { (tickN w t) with capsule=0 }
    if (w'.discs.[0].current=(w'.discs.[0].positions - 1) && aligned w') then t
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

    let d' = Array.append d [| { positions=11; current=0 } |]
    printfn "%d" (tryTimes d' 0)
    0 // return an integer exit code
