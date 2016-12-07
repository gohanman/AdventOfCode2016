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
    let a = s.Substring(i, 1)
    let b = s.Substring(i+1, 1)
    let c = s.Substring(i+2, 1)
    let d = s.Substring(i+3, 1)
    (a<>b && b=c && a=d)

let rec findABBA (s:string) (i:int) =
    match ((i+3) < s.Length) with
    | false -> false
    | true ->
        match (isABBA s i) with
        | true -> true
        | false -> findABBA s (i+1)

let validIP (ip:IPv7) =
    let entryABBA = 
        List.map (fun i -> findABBA i 0) ip.entries
        |> List.fold (fun c i -> c || i) false
    let hyperABBA = 
        List.map (fun i -> findABBA i 0) ip.hypernets
        |> List.fold (fun c i -> c || i) false
    (entryABBA && not(hyperABBA))
             
[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllLines("Input.txt") |> Array.toList
    let ips = List.map (fun i -> parse emptyIP [i]) file
    let validIPs = List.filter validIP ips
    printfn "Valid IPs: %d" (validIPs.Length)
    0 // return an integer exit code
