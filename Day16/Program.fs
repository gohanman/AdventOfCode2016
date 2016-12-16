
let curveDragons (a:string) =
    let b =
        [for c in a -> c]
        |> List.rev
        |> List.map (fun i -> if (i='0') then '1' else '0') 
        |> List.fold (fun c i -> c + i.ToString()) ""
    a + "0" + b

let rec checksum (s:string) =
    let cs =
        [for i in [0..2..(s.Length-1)] -> s.Substring(i, 2)]
        |> List.map (fun i ->
            match i with
            | "00" | "11" -> "1"
            | "01" | "10" -> "0"
            | _ -> ""
        )
        |> List.fold (fun c i -> c + i) ""
    if (cs.Length % 2 = 1) then cs
    else checksum cs

let rec fillData (input:string) (size:int) =
    if (input.Length >= size) then input.Substring(0, size)
    else fillData (curveDragons input) size

let fastCurve (a:string) =
    let sb = System.Text.StringBuilder(a)
    sb.Append("0") |> ignore
    let mutable i = a.Length - 1
    while (i >= 0) do
        sb.Append(if (a.Chars(i)='1') then "0" else "1") |> ignore
        i <- i - 1
    sb.ToString()

let rec fastCheck (s:string) =
    let sb = new System.Text.StringBuilder()
    let mutable i = 0
    while ((i + 1) < s.Length) do
        match (s.Substring(i,2)) with
        | "00" | "11" -> sb.Append("1") |> ignore
        | "01" | "10" -> sb.Append("0") |> ignore
        | _ -> ()
        i <- i + 2
    let cs = sb.ToString()
    if (cs.Length % 2 = 1) then cs
    else fastCheck cs

let fastFill (input:string) (size:int) =
    let mutable ret = input
    while (ret.Length < size) do
        ret <- fastCurve ret
    ret.Substring(0, size)

[<EntryPoint>]
let main argv = 
    let padded = fillData "10001110011110000" 272
    printfn "%d" padded.Length
    let cs = checksum padded
    printfn "%s" cs
    let padded' = fastFill "10001110011110000" 35651584
    printfn "%d" padded'.Length
    let cs' = fastCheck padded'
    printfn "%s" cs'
    0 // return an integer exit code
