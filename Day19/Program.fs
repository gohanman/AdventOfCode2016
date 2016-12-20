
type Round =
    | Even=0
    | Odd=1

type Elf = { number:int; presents:int }

let rec steal (elves:int) (rounds:Round list) =
    match elves with
    | 1 -> rounds
    | _ ->
        let r = if (elves % 2 = 0) then Round.Even else Round.Odd
        steal (elves / 2) (r :: rounds)

let unsteal (elf:int) (r:Round) =
    match r with
    | Round.Even -> (2 * elf) - 1
    | Round.Odd -> (2 * elf) + 1
    | _ -> elf

let nextNonEmpty (elves:Elf array) (pos:int) =
    let mutable i = (pos + 1) % elves.Length
    while (elves.[i].presents = 0) do
        i <- (i + 1) % elves.Length
    i

let circleSteal (size:int) =
    let mutable elves = Array.init size (fun i -> { number=(i+1); presents=1 })
    let mutable count = size
    let mutable cur = 0
    let mutable midpoint = count / 2
    while (count > 1) do
        elves.[midpoint] <- { elves.[midpoint] with presents=0 }
        cur <- nextNonEmpty elves cur
        midpoint <- nextNonEmpty elves midpoint
        if (count % 2 = 1) then
            midpoint <- nextNonEmpty elves midpoint
        count <- count - 1
    elves.[midpoint]

[<EntryPoint>]
let main argv = 
    let rounds = steal 3018458 []
    let elf =
        rounds
        |> List.fold unsteal 1
    printfn "%d" elf
    //let mutable across = Array.init 3018458 (fun i -> { number=(i+1); presents=1 })
    printfn "%A" (circleSteal 3018458)
    0 // return an integer exit code
