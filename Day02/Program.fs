﻿
type Key =
    | One=1
    | Two=2
    | Three=3
    | Four=4
    | Five=5
    | Six=6
    | Seven=7
    | Eight=8
    | Nine=9
    | A=10
    | B=11
    | C=12
    | D=13

type Direction = Up | Down | Left | Right

let LINES="DUURRDRRURUUUDLRUDDLLLURULRRLDULDRDUULULLUUUDRDUDDURRULDRDDDUDDURLDLLDDRRURRUUUDDRUDDLLDDDURLRDDDULRDUDDRDRLRDUULDLDRDLUDDDLRDRLDLUUUDLRDLRUUUDDLUURRLLLUUUUDDLDRRDRDRLDRLUUDUDLDRUDDUDLLUUURUUDLULRDRULURURDLDLLDLLDUDLDRDULLDUDDURRDDLLRLLLLDLDRLDDUULRDRURUDRRRDDDUULRULDDLRLLLLRLLLLRLURRRLRLRDLULRRLDRULDRRLRURDDLDDRLRDLDRLULLRRUDUURRULLLRLRLRRUDLRDDLLRRUDUDUURRRDRDLDRUDLDRDLUUULDLRLLDRULRULLRLRDRRLRLULLRURUULRLLRRRDRLULUDDUUULDULDUDDDUDLRLLRDRDLUDLRLRRDDDURUUUDULDLDDLDRDDDLURLDRLDURUDRURDDDDDDULLDLDLU
LURLRUURDDLDDDLDDLULRLUUUDRDUUDDUDLDLDDLLUDURDRDRULULLRLDDUDRRDRUDLRLDDDURDUURLUURRLLDRURDRLDURUDLRLLDDLLRDRRLURLRRUULLLDRLULURULRRDLLLDLDLRDRRURUUUDUDRUULDLUDLURLRDRRLDRUDRUDURLDLDDRUULDURDUURLLUDRUUUUUURRLRULUDRDUDRLLDUDUDUULURUURURULLUUURDRLDDRLUURDLRULDRRRRLRULRDLURRUULURDRRLDLRUURUDRRRDRURRLDDURLUDLDRRLDRLLLLRDUDLULUDRLLLDULUDUULLULLRLURURURDRRDRUURDULRDDLRULLLLLLDLLURLRLLRDLLRLUDLRUDDRLLLDDUDRLDLRLDUDU
RRDDLDLRRUULRDLLURLRURDLUURLLLUUDDULLDRURDUDRLRDRDDUUUULDLUDDLRDULDDRDDDDDLRRDDDRUULDLUDUDRRLUUDDRUDLUUDUDLUDURDURDLLLLDUUUUURUUURDURUUUUDDURULLDDLDLDLULUDRULULULLLDRLRRLLDLURULRDLULRLDRRLDDLULDDRDDRURLDLUULULRDRDRDRRLLLURLLDUUUDRRUUURDLLLRUUDDDULRDRRUUDDUUUDLRRURUDDLUDDDUDLRUDRRDLLLURRRURDRLLULDUULLURRULDLURRUURURRLRDULRLULUDUULRRULLLDDDDURLRRRDUDULLRRDURUURUUULUDLDULLUURDRDRRDURDLUDLULRULRLLURULDRUURRRRDUDULLLLLRRLRUDDUDLLURLRDDLLDLLLDDUDDDDRDURRL
LLRURUDUULRURRUDURRDLUUUDDDDURUUDLLDLRULRUUDUURRLRRUDLLUDLDURURRDDLLRUDDUDLDUUDDLUUULUUURRURDDLUDDLULRRRUURLDLURDULULRULRLDUDLLLLDLLLLRLDLRLDLUULLDDLDRRRURDDRRDURUURLRLRDUDLLURRLDUULDRURDRRURDDDDUUUDDRDLLDDUDURDLUUDRLRDUDLLDDDDDRRDRDUULDDLLDLRUDULLRRLLDUDRRLRURRRRLRDUDDRRDDUUUDLULLRRRDDRUUUDUUURUULUDURUDLDRDRLDLRLLRLRDRDRULRURLDDULRURLRLDUURLDDLUDRLRUDDURLUDLLULDLDDULDUDDDUDRLRDRUUURDUULLDULUUULLLDLRULDULUDLRRURDLULUDUDLDDRDRUUULDLRURLRUURDLULUDLULLRD
UURUDRRDDLRRRLULLDDDRRLDUDLRRULUUDULLDUDURRDLDRRRDLRDUUUDRDRRLLDULRLUDUUULRULULRUDURDRDDLDRULULULLDURULDRUDDDURLLDUDUUUULRUULURDDDUUUURDLDUUURUDDLDRDLLUDDDDULRDLRUDRLRUDDURDLDRLLLLRLULRDDUDLLDRURDDUDRRLRRDLDDUDRRLDLUURLRLLRRRDRLRLLLLLLURULUURRDDRRLRLRUURDLULRUUDRRRLRLRULLLLUDRULLRDDRDDLDLDRRRURLURDDURRLUDDULRRDULRURRRURLUURDDDUDLDUURRRLUDUULULURLRDDRULDLRLLUULRLLRLUUURUUDUURULRRRUULUULRULDDURLDRRULLRDURRDDDLLUDLDRRRRUULDDD";

let add (k:Key) (i:int) =
    let value = int k
    enum<Key>(value+ i)

let up (k:Key) =
    match k with
    | Key.One | Key.Two | Key.Three -> k
    | Key.Four | Key.Five | Key.Six | Key.Seven | Key.Eight | Key.Nine -> add k -3

let altUp (k:Key) =
    match k with
    | Key.One | Key.Two | Key.Four | Key.Five | Key.Nine -> k
    | Key.Six | Key.Seven | Key.Eight | Key.A | Key.B | Key.C-> add k -4
    | Key.Three | Key.D -> add k -2

let down (k:Key) =
    match k with
    | Key.One | Key.Two | Key.Three | Key.Four | Key.Five | Key.Six -> add k 3
    | Key.Seven | Key.Eight | Key.Nine -> k

let altDown (k:Key) =
    match k with
    | Key.D | Key.A | Key.C | Key.Five | Key.Nine -> k
    | Key.Two | Key.Three | Key.Four | Key.Six | Key.Seven | Key.Eight -> add k 4
    | Key.One | Key.B -> add k 2

let left (k:Key) =
    match k with
    | Key.One | Key.Four | Key.Seven -> k
    | Key.Two | Key.Three | Key.Five | Key.Six | Key.Eight | Key.Nine -> add k -1

let altLeft (k:Key) =
    match k with
    | Key.One | Key.Two | Key.Five | Key.A | Key.D -> k
    | _ -> add k -1

let right (k:Key) =
    match k with
    | Key.Three | Key.Six | Key.Nine -> k
    | Key.One | Key.Two | Key.Four | Key.Five | Key.Seven | Key.Eight -> add k 1

let altRight (k:Key) =
    match k with
    | Key.One | Key.Four | Key.Nine | Key.C | Key.D -> k
    | _ -> add k 1

let lineToList (s:string) =
    [for c in s -> c]
    |> List.filter (fun i -> i = 'U' || i = 'R' || i = 'D' || i = 'L')
    |> List.map (fun i ->
        match i with
        | 'U' -> Up
        | 'R' -> Right
        | 'D' -> Down
        | 'L' -> Left
        | _ -> failwith ("invalid input string: " + i.ToString())
    )

let changeKey (cur:Key) (d:Direction) =
    match d with
    | Up -> up cur
    | Down -> down cur
    | Left -> left cur
    | Right -> right cur

let altChangeKey (cur:Key) (d:Direction) = 
    match d with
    | Up -> altUp cur
    | Down -> altDown cur
    | Left -> altLeft cur
    | Right -> altRight cur

let decodeLine (line:string) (start:Key) keyFunc =
    List.fold keyFunc start (lineToList line)

[<EntryPoint>]
let main argv = 
    let split = LINES.Split([|'\n'|]) |> Array.toList
    let mutable key = Key.Five
    for line in split do
        key <- decodeLine line key changeKey
        printfn "STD:%d" (int key)
    key <- Key.Five
    for line in split do
        key <- decodeLine line key altChangeKey
        printfn "CRAZY:%d" (int key)
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
