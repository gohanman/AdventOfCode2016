
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
        while (cur.Contains(")")) do
            let pass = processString cur ""
            let firstMarker = pass.IndexOf('(')
            cur <- cur.Substring(firstMarker)
            yield firstMarker
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
