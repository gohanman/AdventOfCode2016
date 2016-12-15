
[<Literal>]
let SALT = "zpqevtbw"

let repeatRegex (n:int) =
    let nums = [0..9] |> List.map (fun i -> i.ToString())
    let chars = ['a'..'f'] |> List.map (fun i -> i.ToString())
    let sb = new System.Text.StringBuilder()
    for c in (nums @ chars) do
        sb.Append(c.PadLeft(n, c.[0])) |> ignore
        sb.Append("|") |> ignore
    "(" + sb.ToString().TrimEnd([| '|' |]) + ")"

let TRIO = new System.Text.RegularExpressions.Regex(repeatRegex 3)

let md5 (s:string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    let md5 = System.Security.Cryptography.MD5.Create()
    let result = md5.ComputeHash(bytes)
    System.BitConverter.ToString(result).Replace("-", "").ToLower()

let stretchedMD5 (s:string) =
    let mutable result = s
    for i in [0..2015] do
        result <- md5 s
    result

let rec hasSubKey (hasher:string->string) (index:int) (seq:string) (iter:int) =
    match iter with
    | 1000 -> false
    | _ ->
        let sub = hasher (SALT + index.ToString())
        match (sub.Contains(seq)) with
        | true -> 
            true
        | false -> hasSubKey hasher (index+1) seq (iter+1)

let isKey hasher (index:int) =
    let hashed = hasher (SALT + index.ToString())
    let t = TRIO.Match(hashed)
    match t.Success with
    | true ->
        let str = t.Groups.Item(1).Value
        let seq = "".PadLeft(5, str.[0])
        if (hasSubKey (hasher) (index+1) seq 0) then Some hashed else None
    | false -> None

let rec findKeys hasher (index:int) (keys:string list) (num:int) =
    if (num=keys.Length) then List.rev keys
    else
        match (isKey (hasher) index) with
        | Some k -> findKeys hasher (index+1) ((index.ToString()+":"+k) :: keys) num
        | None -> findKeys hasher (index+1) keys num

[<EntryPoint>]
let main argv = 
    let keys = findKeys (stretchedMD5) 0 [] 64
    keys |> List.iteri (fun i x -> printfn "%d %s" i x)
    0 // return an integer exit code
