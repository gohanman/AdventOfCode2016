
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

[<EntryPoint>]
let main argv = 
    let mutable i = 0
    [0..7]
    |> List.iter (fun j ->
        let digit, nextIndex = nextDigit "ojvtpuvg" i
        printfn "%s" digit
        i <- nextIndex
    )
    0 // return an integer exit code
