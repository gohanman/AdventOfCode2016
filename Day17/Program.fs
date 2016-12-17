
type Position = { x:int; y:int }
type Move =
    | Up=0
    | Down=1
    | Left=2
    | Right=3

[<Literal>]
let MAX_X = 3
[<Literal>]
let MAX_Y = 3

let md5 (s:string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    let md5 = System.Security.Cryptography.MD5.Create()
    let result = md5.ComputeHash(bytes)
    System.BitConverter.ToString(result).Replace("-", "").ToLower()

let canMove (p:Position) (m:Move) =
    match m with
    | Move.Up u -> p.y > 0
    | Move.Down d -> p.y < MAX_Y
    | Move.Left l -> p.x > 0
    | Move.Right r -> p.x < MAX_X
    | _ -> false

let doMove (p:Position) (m:Move) =
    match m with
    | Move.Up u -> { p with y=(p.y - 1) }
    | Move.Down d ->  { p with y=(p.y + 1) }
    | Move.Left l -> { p with x=(p.x - 1) }
    | Move.Right r -> { p with x=(p.x + 1) }
    | _ -> p

let moveStr (m:Move) = 
    match m with
    | Move.Up u -> "U"
    | Move.Down d ->  "D"
    | Move.Left l -> "L"
    | Move.Right r -> "R"
    | _ -> ""

let isOpen (c:char) = (int c) > 97

let moves (p:Position) (s:string) =
    let hash = md5 s
    seq {
        for i in 0..3 do
            let m:Move = enum i
            if ((isOpen (hash.Chars(i))) && (canMove p m)) then yield m
    }

let solved (p:Position) = (p.x=3) && (p.y=3)

let roam (start:Position) (code:string) (shortest:bool) =
    let mutable states = Set.empty.Add((start, ""))
    let mutable searching = true
    while searching do
        let mutable newStates = Set.empty
        for s in states do
            let pos = fst s
            let movesSoFar = snd s
            if (solved pos) then 
                printfn "Solved: (%d) %s" movesSoFar.Length movesSoFar
                if (shortest) then searching <- false
            else
                let next = moves pos (code + movesSoFar)
                for n in next do
                    let s' = (doMove pos n, movesSoFar + (moveStr n))
                    newStates <- newStates.Add(s')
        states <- newStates
        if (states.Count=0) then
            searching <- false
            printfn "Stuck"

[<EntryPoint>]
let main argv = 
    roam { x=0; y=0 } "pgflpeqp" true
    roam { x=0; y=0 } "pgflpeqp" false
    0 // return an integer exit code
