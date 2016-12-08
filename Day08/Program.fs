
type Command =
    | Rect of int*int
    | RotateCol of int*int
    | RotateRow of int*int

let BlankScreen = Array2D.create 50 6 0

let shifter (cur:int) (amt:int) (max:int) =
    let next = cur - amt
    match (next < 0) with
    | false -> next
    | true -> max + next

let doRect (screen:int[,]) (x:int) (y:int) =
    Array2D.mapi (fun a b v -> if (a<x && b<y) then 1 else v) screen

let doRotateCol (screen:int[,]) (col:int) (amt:int) =
    Array2D.mapi (fun a b v -> if (a=col) then screen.[a, (shifter b amt 6)] else v) screen

let doRotateRow (screen:int[,]) (row:int) (amt:int) =
    Array2D.mapi (fun a b v -> if (b=row) then screen.[(shifter a amt 50), b] else v) screen

let applyCommand (screen:int[,]) (c:Command) =
    match c with
    | Rect (x,y) -> doRect screen x y
    | RotateCol (x,y) -> doRotateCol screen x y
    | RotateRow (x,y) -> doRotateRow screen x y

let rec applyCommands (screen:int[,]) (c:Command list) =
    match c.Length with
    | 0 -> screen
    | _ ->
        let newScreen = applyCommand screen c.Head
        applyCommands newScreen c.Tail

let lineToCmd (s:string) =
    if (s.Substring(0, 5) = "rect ") then
        let rest = s.Substring(5).Split([|'x'|]) |> Array.map (fun i -> System.Int32.Parse(i))
        Some (Rect (rest.[0], rest.[1]))
    elif (s.Substring(0,16) = "rotate column x=") then
        let rest = 
            s.Substring(16).Split([|" by "|], System.StringSplitOptions.None) 
            |> Array.map (fun i -> System.Int32.Parse(i))
        Some (RotateCol (rest.[0], rest.[1]))
    elif (s.Substring(0,13) = "rotate row y=") then
        let rest = 
            s.Substring(13).Split([|" by "|], System.StringSplitOptions.None) 
            |> Array.map (fun i -> System.Int32.Parse(i))
        Some (RotateRow (rest.[0], rest.[1]))
    else
        None
    
let sumScreen (screen:int[,]) =
    seq {
        for row in 0 .. ((Array2D.length1 screen)-1) do
            for col in 0 .. ((Array2D.length2 screen)-1) do
                yield screen.[row,col]
    }
    |> Seq.fold (fun c i -> c+i) 0

let printScreen (screen:int[,]) =
    for col in 0..5 do
        for row in 0..49 do
            printf "%s" (if (screen.[row,col]=1) then "#" else ".")
        printfn ""

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let cmds = List.map lineToCmd file |> List.choose id
    let result = applyCommands BlankScreen cmds
    printScreen result
    printfn "Sum: %d" (sumScreen result)
    0 // return an integer exit code
