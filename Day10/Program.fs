
open System.Text.RegularExpressions

type Command = 
    | Ingest of (int * int)
    | GiveBB of (int * int * int)
    | GiveOO of (int * int * int)
    | GiveBO of (int * int * int)
    | GiveOB of (int * int * int)
type Chip = Chip of int
type Bot = { id:int; instructions:Command list; chips:Chip list }
type Output = { id:int; chips:Chip list }
type Factory = { bots:Bot list; outputs:Output list }

let addOutput (f:Factory) (o:Output) =
    { f with outputs=(o :: f.outputs) }

let hasOutput (f:Factory) (id:int) =
    let has = List.filter (fun o -> id=o.id) f.outputs
    (has.Length > 0)

let replaceOutput (ol:Output list) (o:Output) =
    ol
    |> List.map (fun i -> if (i.id=o.id) then o else i)

let addBot (f:Factory) (b:Bot) =
    { f with bots=(b :: f.bots) }

let hasBot (f:Factory) (id:int) =
    let has = List.filter (fun (b:Bot) -> id=b.id) f.bots
    (has.Length > 0)

let replaceBot (bl:Bot list) (b:Bot) =
    bl
    |> List.map (fun i -> if (i.id=b.id) then b else i)

let addChip (b:Bot) (c:Chip) =
    { b with chips=(c :: b.chips) }

let outputChip (o:Output) (c:Chip) =
    { o with chips=(c :: o.chips) }

let removeChip (b:Bot) (c:Chip) =
    let removed = b.chips |> List.filter (fun i -> i <> c)
    { b with chips=removed }

let readyBots (f:Factory) =
    f.bots |> List.filter (fun b -> b.chips.Length = 2)

let takeLow (f:Factory) (id:int) =
    let bot = f.bots |> List.filter (fun i -> i.id=id) |> List.head
    let low = List.min bot.chips
    let newBot = removeChip bot low
    (low, { f with bots=(replaceBot f.bots newBot) })

let takeHigh (f:Factory) (id:int) =
    let bot = f.bots |> List.filter (fun i -> i.id=id) |> List.head
    let high = List.max bot.chips
    let newBot = removeChip bot high
    (high, { f with bots=(replaceBot f.bots newBot) })

let giveToBot (f:Factory) (id:int) (c:Chip) =
    let bot = f.bots |> List.filter (fun i -> i.id=id) |> List.head
    let newBot = addChip bot c
    { f with bots=(replaceBot f.bots newBot) }

let giveToOutput (f:Factory) (id:int) (c:Chip) =
    let out = f.outputs |> List.filter (fun i -> i.id=id) |> List.head
    let newOut = outputChip out c
    { f with outputs=(replaceOutput f.outputs newOut) }

let queueCmd (f:Factory) (id:int) (cmd:Command) =
    let bot = f.bots |> List.filter (fun i -> i.id=id) |> List.head
    let newBot = { bot with instructions=(cmd :: bot.instructions) }
    { f with bots=(replaceBot f.bots newBot) }

let toInt (s:string) = System.Int32.Parse(s)

let strToCmd (s:string) =
    let valRX = new Regex("value (\d+) goes to bot (\d+)")
    let giveRX = new Regex("bot (\d+) gives low to ([a-z]+) (\d+) and high to ([a-z]+) (\d+)")
    let valMatch = valRX.Match(s)
    let giveMatch = giveRX.Match(s)
    if (valMatch.Success) then
        let g = valMatch.Groups
        Some (Command.Ingest ((toInt (g.Item(2).Value)), (toInt (g.Item(1).Value))))
    elif (giveMatch.Success) then
        let g = giveMatch.Groups
        let bot = toInt (g.Item(1).Value)
        let low = toInt(g.Item(3).Value)
        let high = toInt(g.Item(5).Value)
        let pair = ((g.Item(2).Value), (g.Item(4).Value))
        match pair with
        | ("bot", "bot") -> Some (Command.GiveBB (bot, low, high))
        | ("bot", "output") -> Some (Command.GiveBO (bot, low, high))
        | ("output", "output") -> Some (Command.GiveOO (bot, low, high))
        | ("output", "bot") -> Some (Command.GiveOB (bot, low, high))
        | _ -> None
    else
        None

let worldWithBot (f:Factory) (b:int) =
    if (hasBot f b) then 
        f 
    else 
        addBot f ({ id=b; chips=[]; instructions=[] })

let runCommand (f:Factory) (c:Command) =
    match c with
    | Ingest (b,v) ->
        let newWorld = worldWithBot f b
        giveToBot f b (Chip v) 
    | GiveBB (b,_,_) | GiveBO (b,_,_)| GiveOB (b,_,_)| GiveOO (b,_,_) ->
        let newWorld = worldWithBot f b
        queueCmd newWorld b c

let botCommand (f:Factory) (c:Command) =
    match c with
    | Ingest (_,_) -> f
    | GiveBB (b,l,h) ->
        let lowChip, f1 = takeLow f b 
        let highChip, f2 = takeHigh f1 b
        let f3 = giveToBot f2 l lowChip
        giveToBot f3 h highChip
    | GiveBO (b,l,h) ->
        let lowChip, f1 = takeLow f b 
        let highChip, f2 = takeHigh f1 b
        let f3 = giveToBot f2 l lowChip
        giveToOutput f3 h highChip
    | GiveOO (b,l,h) ->
        let lowChip, f1 = takeLow f b 
        let highChip, f2 = takeHigh f1 b
        let f3 = giveToOutput f2 l lowChip
        giveToOutput f3 h highChip
    | GiveOB (b,l,h) ->
        let lowChip, f1 = takeLow f b 
        let highChip, f2 = takeHigh f1 b
        let f3 = giveToOutput f2 l lowChip
        giveToBot f3 h highChip

let runWorld (cmds:Command list) =
    let mutable world = { bots=[]; outputs=[] }
    for cmd in cmds do
        world <- runCommand world cmd

    while ((readyBots world).Length > 0) do
        for bot in (readyBots world) do
            if (bot.instructions.Length > 0) then
                world <- botCommand world bot.instructions.Head
    world

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
