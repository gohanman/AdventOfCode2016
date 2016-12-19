
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

let findAcross (cur:int) (num:int) =
    printfn "%d %d" cur num
    let half = num / 2
    let even = (num % 2) = 0
    match even with
    | true ->
        if (cur < half) then cur + half else cur - half
    | false ->
        if (cur < half) then cur + half
        elif (cur = half) then 0
        else cur - (half + 1)

let stealAcross (elves:Elf array) (cur:int) =
    let opp = findAcross cur elves.Length
    if (opp = 0) then elves.[1..]
    else
        Array.append elves.[..(opp-1)] elves.[(opp+1)..]

let rec allAcross (elves:Elf array) (cur:int) =
    if (elves.Length = 1) then elves.[0]
    else
        let lessOne = stealAcross elves cur
        let cur' = if (cur + 1 >= lessOne.Length) then 0 else cur + 1
        allAcross lessOne cur'

[<EntryPoint>]
let main argv = 
    let rounds = steal 3018458 []
    let elf =
        rounds
        |> List.fold unsteal 1
    printfn "%d" elf
    let across = Array.init 3018458 (fun i -> { number=(i+1); presents=1 })
    printfn "%A" (allAcross across 0)
    0 // return an integer exit code
