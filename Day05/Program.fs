
let goodHash (s:string) =
    s.Substring(0, 5) = "00000"

let getHash (s:string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(s)
    let md5 = System.Security.Cryptography.MD5.Create()
    let result = md5.ComputeHash(bytes)
    System.BitConverter.ToString(result).Replace("-", "")

let rec nextDigit (doorID:string) (index:int) =
    let hashString = getHash (doorID + (index.ToString()))
    match (goodHash hashString) with
    | true -> (hashString.Substring(5, 1), index+1)
    | false -> nextDigit doorID (index+1)

let secondGen (doorID:string) (index:int) (map:Map<int,string>) =
    let hashString = getHash (doorID + (index.ToString()))
    let parsed,pos = System.Int32.TryParse(hashString.Substring(5, 1))
    let value = hashString.Substring(6, 1)
    printfn "%d %s %s" pos value hashString
    if (parsed && (pos > 7 || map.ContainsKey(pos))) then
        map
    elif not(parsed) then
        map
    else
        let newMap = map.Add(pos, value)
        newMap

[<EntryPoint>]
let main argv = 
    let mutable i = 0
    let mutable charMap:Map<int,string> = Map.empty
    let mutable searching = true
    printfn "%s" (System.Int32.TryParse("A").ToString())
    while searching do
        let digit, nextIndex = nextDigit "ojvtpuvg" i
        let newMap = secondGen "ojvtpuvg" (nextIndex-1) charMap
        charMap <- newMap
        i <- nextIndex
        printfn "First Gen: %s" digit
        if (charMap.Count >= 8) then
            printfn "Second Gen: %A" (Map.toList charMap)
            searching <- false
        else
            printfn "Parial Second: %A" (Map.toList charMap)
    0 // return an integer exit code
