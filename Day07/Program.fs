type IPv7 = { entries:string list; hypernets:string list }

let emptyIP = { entries=[]; hypernets=[] }
let matcher = System.Text.RegularExpressions.Regex("^(.*?)\[(.+?)\](.*)$")

let rec parse (ip:IPv7) (remaining:string list) =
    match (remaining.Length) with
    | 0 -> ip
    | _ ->
        let cur = remaining.Head
        let m = matcher.Match(cur)
        match (m.Success) with
        | false -> 
            let newEntries = cur :: ip.entries
            let newIP = { ip with entries=newEntries }
            parse newIP remaining.Tail
        | true ->
            let g = m.Groups
            let hyper = g.Item(2).Value
            let others = 
                [g.Item(1).Value; g.Item(3).Value] 
                |> List.filter (fun i -> i.Length > 0)
            let newHyper = hyper :: ip.hypernets
            let newRemain = List.append remaining.Tail others
            let newIP = { ip with hypernets=newHyper }
            parse newIP newRemain

let isABBA (s:string) (i:int) =
    (s.[i]<>s.[i+1] && s.[i+1]=s.[i+2] && s.[i]=s.[i+3])

let rec findABBA (s:string) (i:int) =
    match ((i+3) < s.Length) with
    | false -> false
    | true ->
        match (isABBA s i) with
        | true -> true
        | false -> findABBA s (i+1)

let isABA (s:string) (i:int) =
    (s.[i]<>s.[i+1] && s.[i]=s.[i+2])

let findABAs (s:string) =
    seq {
        for i in 0..(s.Length-3) do
            if (isABA s i) then yield (s.Substring(i, 3))
    }

let rec allABAs (sl:string list) (ret:string list) =
    match sl.Length with
    | 0 -> ret
    | _ ->
        let ABAs = findABAs sl.Head |> Seq.toList
        allABAs sl.Tail (List.append ret ABAs)

let isReverse (a:string) (b:string) =
    match (a.Length = 3 && b.Length =3) with
    | false -> false
    | true ->
        ((isABA a 0) && (isABA b 0) && a.[0]=b.[1] && a.[1]=b.[0])

let hasReverse (s:string) (opts:string list) =
    opts
    |> List.map (fun i -> isReverse i s)
    |> List.fold (fun c i -> c || i) false

let validIP (ip:IPv7) =
    let entryABBA = 
        List.map (fun i -> findABBA i 0) ip.entries
        |> List.fold (fun c i -> c || i) false
    let hyperABBA = 
        List.map (fun i -> findABBA i 0) ip.hypernets
        |> List.fold (fun c i -> c || i) false
    (entryABBA && not(hyperABBA))

let validSSL (ip:IPv7) =
    let entryABA = allABAs ip.entries []
    let hyperABA = allABAs ip.hypernets []
    let reverses = List.map (fun i -> hasReverse i hyperABA) entryABA
    List.fold (fun c i -> c || i) false reverses
             
[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let ips = List.map (fun i -> parse emptyIP [i]) file
    let validIPs = List.filter validIP ips
    printfn "Valid IPs: %d" (validIPs.Length)
    let sslIPs = List.filter validSSL ips
    printfn "Valid IPs: %d" (sslIPs.Length)
    0 // return an integer exit code
