
type Copy = { src:string; dest: string }
type Increment = { register: string }
type Decrement = { register: string }
type JumpNZ = { register: string; amount:string }
type Toggle = { register: string; }
type Addition = { register: string; amount:string }
type Multiply = { register: string; amount:string }
type Command =
    | Cpy of Copy
    | Inc of Increment
    | Dec of Decrement
    | Jnz of JumpNZ
    | Tgl of Toggle
    | Add of Addition
    | Mul of Multiply

type Computer = { a:int; b:int; c:int; d:int; pc:int; cmds:Command array }

let validRegister (s:string) =
    if (s="a" || s="b" || s="c" || s="d") then true
    else false

let isInt (s:string) =
    let is,_ = System.Int32.TryParse(s)
    is

let either (s:string) =
    validRegister s || isInt s

let strToCmd (s:string) : (Command Option) =
    match (s.Substring(0, 3)) with
    | "cpy" -> 
        let parts = s.Split([|' '|], 3)
        if (validRegister (parts.[2]) && (either parts.[1])) then
            Some (Cpy { src=parts.[1]; dest=parts.[2] })
        else
            None
    | "inc" ->
        let parts = s.Split([|' '|], 2)
        if (validRegister (parts.[1])) then Some (Inc { register=(parts.[1]) })
        else None
    | "dec" ->
        let parts = s.Split([|' '|], 2)
        if (validRegister (parts.[1])) then Some (Dec { register=(parts.[1]) })
        else None
    | "jnz" ->
        let parts = s.Split([|' '|], 3)
        if ((either parts.[1]) && (either parts.[2])) then
            Some (Jnz { register=parts.[1]; amount=parts.[2] })
        else
            None
    | "tgl" ->
        let parts = s.Split([|' '|], 2)
        if (either parts.[1]) then
            Some (Tgl { register=parts.[1] })
        else
            None
    | "add" ->
        let parts = s.Split([|' '|], 3)
        if ((either parts.[1]) && (either parts.[2])) then
            Some (Add { register=parts.[1]; amount=parts.[2] })
        else
            None
    | "mul" ->
        let parts = s.Split([|' '|], 3)
        if ((either parts.[1]) && (either parts.[2])) then
            Some (Mul { register=parts.[1]; amount=parts.[2] })
        else
            None
    | _ -> None

let setRegister (c:Computer) (r:string) (v:int) =
    match r with
    | "a" -> { c with a=v }
    | "b" -> { c with b=v }
    | "c" -> { c with c=v }
    | "d" -> { c with d=v }
    | _ -> c

let getRegister (c:Computer) (r:string) =
    match r with
    | "a" -> c.a
    | "b" -> c.b
    | "c" -> c.c
    | "d" -> c.d
    | _ -> 0

let getVal (c:Computer) (s:string) =
    if (isInt s) then System.Int32.Parse(s)
    else getRegister c s

let doCopy (c:Computer) (cmd:Copy) =
    if (not (validRegister cmd.dest)) then { c with pc=(c.pc + 1) }
    else
        let copyVal = getVal c cmd.src
        let c' = setRegister c cmd.dest copyVal
        { c' with pc=(c'.pc + 1) }

let doIncrement (c:Computer) (cmd:Increment) =
    if (not (validRegister cmd.register)) then { c with pc=(c.pc + 1) }
    else
        let cur = getRegister c cmd.register
        let c' = setRegister c cmd.register (cur+1)
        { c' with pc=(c'.pc + 1) }

let doDecrement (c:Computer) (cmd:Decrement) =
    if (not (validRegister cmd.register)) then { c with pc=(c.pc + 1) }
    else
        let cur = getRegister c cmd.register
        let c' = setRegister c cmd.register (cur-1)
        { c' with pc=(c'.pc + 1) }

let doJNZ (c:Computer) (cmd:JumpNZ) =
    let cur = getVal c cmd.register
    let jump = getVal c cmd.amount
    if (cur=0) then
        { c with pc=(c.pc + 1) }
    else
        { c with pc=(c.pc + jump ) }

let doToggle (c:Computer) (cmd:Toggle) =
    let offset = getVal c cmd.register
    let index = c.pc + offset
    if (index >= c.cmds.Length) then
        { c with pc=(c.pc + 1) }
    else
        let cmds' = c.cmds
        cmds'.[index] <-
            match (cmds'.[index]) with
            | Cpy i -> Jnz { register=i.src; amount=i.dest }
            | Jnz i -> Cpy { src=i.register; dest=i.amount }
            | Inc i -> Dec { register=i.register }
            | Dec i -> Inc { register=i.register }
            | Tgl i -> Inc { register=i.register }
        { c with cmds=cmds'; pc=(c.pc + 1) }

let doAdd (c:Computer) (cmd:Addition) =
    let amount = getVal c cmd.amount
    let cur = getRegister c cmd.register
    let c' = setRegister c cmd.register (cur + amount)
    { c' with pc=(c.pc + 1) }

let doMultiply (c:Computer) (cmd:Multiply) =
    let amount = getVal c cmd.amount
    let cur = getRegister c cmd.register
    let c' = setRegister c cmd.register (cur * amount)
    { c' with pc=(c.pc + 1) }
    
let runCommand (c:Computer) (cmd:Command) =
    match cmd with
    | Cpy i -> doCopy c i
    | Inc i -> doIncrement c i
    | Dec i -> doDecrement c i
    | Jnz i -> doJNZ c i
    | Tgl i -> doToggle c i
    | Add i -> doAdd c i
    | Mul i -> doMultiply c i

let rec runProgram (c:Computer) =
    if (c.pc >= c.cmds.Length) then
        c
    else
        let c' = runCommand c c.cmds.[c.pc]
        runProgram c'

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt")
    let cmds = Array.map strToCmd file |> Array.choose id
    let start = { a=7; b=0; c=0; d=0; pc=0; cmds=cmds }
    let result = runProgram start
    printfn "%A" result

    let file' = System.IO.File.ReadAllLines("Input2.txt")
    let cmds' = Array.map strToCmd file' |> Array.choose id
    let start' = { a=12; b=0; c=0; d=0; pc=0; cmds=cmds' }
    let result' = runProgram start'
    printfn "%A" result'
    0 // return an integer exit code
