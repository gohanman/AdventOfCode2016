
type Repeater = { length:int; count:int }

let regex = System.Text.RegularExpressions.Regex("^(.*)\((\d+)x(\d+)$")

let getPreAndRepeat (left:string) =
    let m = regex.Match(left)
    let repeat = { 
        length=(System.Int32.Parse(m.Groups.Item(2).Value)); 
        count=(System.Int32.Parse(m.Groups.Item(3).Value))
    }
    let prefix = m.Groups.Item(1).Value
    (prefix, repeat)

let parts (s:string) =
    let p = s.Split([|')'|], 2)
    let left = p.[0]
    let right = p.[1]
    (left, right)

let repeatText (right:string) (prefix:string) (repeat:Repeater) =
    let sb = new System.Text.StringBuilder(prefix)
    for i in 0..(repeat.count-1) do
        sb.Append(right.Substring(0, repeat.length)) |> ignore
    sb.ToString()

let rec processString (remaining:string) (complete:string) =
    match (remaining.Contains(")")) with
    | false -> complete + remaining
    | true ->
        let left, right = parts remaining
        let prefix,repeat = getPreAndRepeat left
        let newRemain = right.Substring(repeat.length)
        let repeated = repeatText right (complete+prefix) repeat

        processString newRemain repeated

let countSuperEncrypted (s:string) =
    seq {
        let mutable cur = s
        let sb = new System.Text.StringBuilder()
        while (cur.Contains(")")) do
            printfn "%d" cur.Length
            let firstMarker = cur.IndexOf('(')
            if (firstMarker > 0) then yield firstMarker
            cur <- cur.Substring(firstMarker+1)
            let endMarker = cur.IndexOf(')')
            let pair = cur.Substring(0, endMarker)
            cur <- cur.Substring(endMarker+1)
            let pts = pair.Split([|'x'|], 2)
            let rp = {
                length=(System.Int32.Parse(pts.[0]));
                count=(System.Int32.Parse(pts.[1]));
            }
            let repeated = repeatText (cur.Substring(0, rp.length)) "" rp
            cur <- sb.Clear().Append(repeated).Append(cur.Substring(rp.length)).ToString()

        yield cur.Length
    }

[<EntryPoint>]
let main argv = 
    let file = System.IO.File.ReadAllText("Input.txt")
    let res = processString file ""
    printfn "%d" res.Length
    let longOne =
        countSuperEncrypted file
        |> Seq.fold (fun c i -> c+i) 0
    printfn "%d" longOne
    0 // return an integer exit code
