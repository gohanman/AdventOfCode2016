module Main 

    open PriorityQueue

    type Tile =
        | Value of int
        | Wall of bool
        | Open of bool
    type Coord = { x:int; y:int }
    type World = { tiles:Tile[,]; position:Coord; collected:Set<int> }
    type Path = { src:int; dest:int }
    type AStar = { 
        f:Map<Coord,int>; 
        g:Map<Coord,int>; 
        visited:Set<Coord>;
        available:PriorityQueue.Queue<Coord>;
        world:World;
        hueristic:Coord -> Coord -> int;
    }

    let canMove (w:World) (c:Coord) =
        if (c.x < 0 || c.y < 0) then false
        elif (c.x >= (Array2D.length1 w.tiles) || c.y >= (Array2D.length2 w.tiles)) then false
        else
            match (w.tiles.[c.x,c.y]) with
            | Value i -> true
            | Open i -> true
            | Wall i -> false

    let moves (w:World) =
        seq {
            if (canMove w { w.position with x=(w.position.x - 1) }) then
                yield { w.position with x=(w.position.x - 1) }
            if (canMove w { w.position with x=(w.position.x + 1) }) then
                yield { w.position with x=(w.position.x + 1) }
            if (canMove w { w.position with y=(w.position.y - 1) }) then
                yield { w.position with y=(w.position.y - 1) }
            if (canMove w { w.position with y=(w.position.y + 1) }) then
                yield { w.position with y=(w.position.y + 1) }
        }

    let move (w:World) (c:Coord) =
        let newpos = w.tiles.[c.x,c.y]
        match newpos with
        | Value i ->
            if (i > 0) then 
                { w with position=c; collected=(w.collected.Add(i)) }
            else
                { w with position=c }
        | Open i ->
            { w with position=c }
        | Wall i -> w

    let init (x:int) (y:int) =
        Tile.Wall true

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

    let addChild (goal:Coord) (parent:Coord) (state:AStar) (child:Coord) =
        let gval = state.g.Item(parent) + 1
        let g' = state.g.Add(child, gval)
        let fval = gval + (state.hueristic child goal)
        let f' = state.f.Add(child, fval)
        let a' = PriorityQueue.push state.available child fval
        { state with g=g'; f=f'; available=a' }

    let rec shortest (state:AStar) (goal:Coord) =
        let cur,avail' = PriorityQueue.pop state.available
        if (cur = goal) then state.g.Item(cur)
        else
            let v' = state.visited.Add(cur)
            let state' = { state with visited=v'; available=avail' }
            let children = 
                moves { state.world with position=cur }
                |> Seq.filter (fun i -> not (v'.Contains(i)))
            let state'' =
                children
                |> Seq.fold (addChild goal cur) state'
            shortest state'' goal

    let solved (w:World list) (target:int) =
        w
        |> List.filter (fun i -> i.collected.Contains(target))
        |> List.isEmpty |> not

    let rec walk (w:World list) (target:int) (visited:Set<Coord>) (steps:int) =
        printfn "%d" steps
        if (solved w target) then
            steps
        else
            let mutable v' = visited
            let mutable w' = []
            for world in w do
                v' <- v'.Add(world.position)
                let opts = moves world
                let newWorlds =
                    opts
                    |> Seq.map (fun i -> move world i)
                    |> Seq.filter (fun i -> not (visited.Contains(i.position)))
                    |> Seq.toList
                w' <- w' @ newWorlds
            if (w'.Length = 0) then
                printfn "Stuck at %d" steps
                0
            else
                walk w' target v' (steps + 1)

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
                        g=Map.empty.Add(s, 0);
                        f=Map.empty.Add(s, distance s d);
                        available=(PriorityQueue.push pq s (distance s d));
                        hueristic=distance;
                        world={world with position=s };
                        visited=Set.empty;
                    }
                    let steps = shortest astar d
                    cache <- cache.Add((s,d), steps)
                    total <- total + steps
            explored <- (path', total) :: explored
            printfn "Checked %d paths" explored.Length
        let best = List.minBy snd explored
        printfn "Best: %A, Steps: %d" (fst best) (snd best)
        0 // return an integer exit code
