
type LeftRight = Left=0 | Right=1

type SwapPosition = { x:int; y:int }
type SwapLetter = { x:char; y:char }
type Rotate = { direction:LeftRight; times:int }
type RotateByLetter = { x:char }
type Reverse = { x:int; y:int }
type Move = { x:int; y:int }

type Op = 
    | SpOp of SwapPosition
    | SlOp of SwapLetter
    | RtOp of Rotate
    | RblOp of RotateByLetter
    | RvOp of Reverse
    | MvOp of Move 

let swapPos (str:string) (sp:SwapPosition) =
    [for c in str -> c]
    |> List.mapi (fun i c ->
        if (i = sp.x) then str.Chars(sp.y)
        elif (i = sp.y) then str.Chars(sp.x)
        else c
    )
    |> Array.ofList
    |> System.String.Concat

let swapLtr (str:string) (sl:SwapLetter) =
    let x = str.IndexOf(sl.x)
    let y = str.IndexOf(sl.y)
    swapPos str ({ SwapPosition.x=x; y=y })

let rotate (str:string) (r:Rotate) =
    let t' = r.times % str.Length
    [for c in str -> c]
    |> List.mapi (fun i c ->
        match r.direction with
        | LeftRight.Right -> str.Chars((i + str.Length - t') % str.Length)
        | LeftRight.Left -> str.Chars((i + t') % str.Length)
        | _ -> c
    )
    |> Array.ofList
    |> System.String.Concat

let rotateLtr (str:string) (rl:RotateByLetter) =
    let index = str.IndexOf(rl.x)
    let times = if (index >= 4) then 2 + index else 1 + index
    rotate str ({ direction=LeftRight.Right; times=times })

let reverse (str:string) (r:Reverse) =
    [for c in str -> c]
    |> List.mapi (fun i c ->
        if (i >= r.x && i <= r.y) then
            str.Chars(r.y - (i - r.x))
        else
            c
    )
    |> Array.ofList
    |> System.String.Concat

let move (str:string) (m:Move) =
    let clipped = str.Substring(0, m.x) + str.Substring(m.x + 1)
    if (m.y < clipped.Length) then 
        clipped.Substring(0, m.y) + str.Substring(m.x, 1) + clipped.Substring(m.y)
    else
        clipped.Substring(0, m.y) + str.Substring(m.x, 1)

let applyCommand (str:string) (o:Op) =
    match o with
    | SpOp sp -> swapPos str sp
    | SlOp sl -> swapLtr str sl
    | RtOp r -> rotate str r
    | RblOp rbl -> rotateLtr str rbl
    | RvOp r -> reverse str r
    | MvOp m -> move str m

let undoCommand (str:string) (o:Op) =
    match o with
    | SpOp sp -> swapPos str { x=sp.y; y=sp.x }
    | SlOp sl -> swapLtr str { x=sl.y; y=sl.x }
    | RtOp r -> 
        match (r.direction) with
        | LeftRight.Left -> rotate str { r with direction=LeftRight.Right }
        | LeftRight.Right -> rotate str { r with direction=LeftRight.Left }
        | _ -> str
    | RblOp rbl ->
        let mutable s' = str
        while ((rotateLtr s' rbl) <> str) do
            s' <- rotate s' { direction=LeftRight.Left; times=1 }
        s'
    | RvOp r -> reverse str r
    | MvOp m -> move str  { Move.x=m.y; y=m.x }

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let strToOp (str:string) =
    match str with
    | Prefix "swap position " sfx ->
        let parts = sfx.Split([|' '|], 4)
        Some (SpOp { SwapPosition.x=(System.Int32.Parse(parts.[0])); y=(System.Int32.Parse(parts.[3])) })
    | Prefix "swap letter " sfx ->
        let parts = sfx.Split([|' '|], 4)
        Some (SlOp { SwapLetter.x=(parts.[0].Chars(0)); y=(parts.[3].Chars(0)) })
    | Prefix "rotate based on position of letter " sfx ->
        Some (RblOp { RotateByLetter.x=(sfx.Chars(0)) })
    | Prefix "rotate left " sfx ->
        let parts = sfx.Split([|' '|], 2)
        Some (RtOp { direction=LeftRight.Left; times=(System.Int32.Parse(parts.[0])) })
    | Prefix "rotate right " sfx ->
        let parts = sfx.Split([|' '|], 2)
        Some (RtOp { direction=LeftRight.Right; times=(System.Int32.Parse(parts.[0])) })
    | Prefix "reverse positions " sfx ->
        let parts = sfx.Split([|' '|], 3)
        Some (RvOp { Reverse.x=(System.Int32.Parse(parts.[0])); y=(System.Int32.Parse(parts.[2])) })
    | Prefix "move position " sfx ->
        let parts = sfx.Split([|' '|], 4)
        Some (MvOp { Move.x=(System.Int32.Parse(parts.[0])); y=(System.Int32.Parse(parts.[3])) })
    | _ -> None

let rec applyAll (str:string) (ops:Op list) =
    if (ops.Length = 0) then str
    else
        let str' = applyCommand str ops.Head
        applyAll str' ops.Tail

let rec undoAll (str:string) (ops:Op list) =
    if (ops.Length = 0) then str
    else
        let str' = undoCommand str ops.Head
        undoAll str' ops.Tail

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt")
    let ops = 
        file
        |> Array.map strToOp
        |> Array.choose id
        |> Array.toList
    let res = applyAll "abcdefgh" ops
    printfn "Scrambled: %s" res
    let rev = undoAll "fbgdceah" (List.rev ops)
    printfn "Unscrambled: %s" rev
    0 // return an integer exit code
