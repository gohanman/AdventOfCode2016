module Main 

    open PriorityQueue
    open AStar

    type Tile =
        | Value of int
        | Wall of bool
        | Open of bool
    type Coord = { x:int; y:int }
    type World = { tiles:Tile[,]; position:Coord; collected:Set<int> }
    type Path = { src:int; dest:int }

    let canMove (w:World) (c:Coord) =
        if (c.x < 0 || c.y < 0) then false
        elif (c.x >= (Array2D.length1 w.tiles) || c.y >= (Array2D.length2 w.tiles)) then false
        else
            match (w.tiles.[c.x,c.y]) with
            | Value i -> true
            | Open i -> true
            | Wall i -> false

    let moves (w:World) (position:Coord) =
        seq {
            if (canMove w { position with x=(position.x - 1) }) then
                yield { position with x=(position.x - 1) }
            if (canMove w { position with x=(position.x + 1) }) then
                yield { position with x=(position.x + 1) }
            if (canMove w { position with y=(position.y - 1) }) then
                yield { position with y=(position.y - 1) }
            if (canMove w { position with y=(position.y + 1) }) then
                yield { position with y=(position.y + 1) }
        }

    let fileToTiles (filename:string) =
        let lines = System.IO.File.ReadAllLines(filename)
        let width = lines.[0].Length - 1
        let height = lines.Length - 1
        let tiles =
            Array2D.init width height (fun x y -> Tile.Wall true)
        for y in [0..height] do
            for x in [0..width] do
                let cur = int (lines.[y].Chars(x))
                if (cur >= 48 && cur <= 57) then
                    tiles.[x,y] <- Tile.Value (cur - 48)
                elif (cur = 46) then
                    tiles.[x,y] <- Tile.Open true
        tiles

    let findVal (tiles:Tile[,]) (target:int) =
        let width = (Array2D.length1 (tiles)) - 1
        let height = (Array2D.length2 (tiles)) - 1
        let mutable pos = { x=0; y=0 }
        let target' = Tile.Value target
        for i in [0..width] do
            for j in [0..height] do
                if (tiles.[i,j] = target') then
                    pos <- { x=i; y=j }
        pos

    let distance (d1:Coord) (d2:Coord) =
        (abs (d1.x - d2.x)) + (abs (d1.y - d2.y))

    let distrib e L =
        let rec aux pre post = 
            seq {
                match post with
                | [] -> yield (L @ [e])
                | h::t -> yield (List.rev pre @ [e] @ post)
                          yield! aux (h::pre) t 
            }
        aux [] L

    let rec perms = function 
        | [] -> Seq.singleton []
        | h::t -> Seq.collect (distrib h) (perms t)

    [<EntryPoint>]
    let main argv = 
        let tiles = fileToTiles "Input.txt"
        let world = { tiles=tiles; position={x=0; y=0}; collected=Set.empty }
        let paths1 =
            perms [1..7]
            |> Seq.map (fun i -> 0 :: i)
        let paths2 =
            perms [1..7]
            |> Seq.map (fun i -> [0] @ i @ [0])
        let mutable cache = Map.empty
        let mutable explored = []
        for path in paths2 do
            let path' = Array.ofList path
            let mutable total = 0
            for i in [0..(path'.Length - 2)] do
                let s = findVal tiles path'.[i]
                let d = findVal tiles path'.[i+1]
                if (cache.ContainsKey(s,d)) then
                    total <- total + cache.Item(s,d)
                else 
                    let pq = PriorityQueue.init (distance d)
                    let astar = {
                        State.g=Map.empty.Add(s, 0);
                        f=Map.empty.Add(s, distance s d);
                        available=(PriorityQueue.push pq s (distance s d));
                        hueristic=(distance d);
                        visited=Set.empty;
                        expand=(moves world);
                    }
                    let steps = AStar.shortest astar d
                    cache <- cache.Add((s,d), steps)
                    total <- total + steps
            explored <- (path', total) :: explored
            printfn "Checked %d paths" explored.Length
        let best = List.minBy snd explored
        printfn "Best: %A, Steps: %d" (fst best) (snd best)
        0 // return an integer exit code
