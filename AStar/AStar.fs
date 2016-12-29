namespace AStar

open PriorityQueue

    type State<'a when 'a : comparison> = { 
        // map state => estimated total distance
        f:Map<'a,int>; 
        // map state => actual distance so far
        g:Map<'a,int>; 
        // previously visited states
        visited:Set<'a>;
        // unexplored states
        available:Queue<'a>;
        // estimate distance from a given state
        hueristic:'a -> int;
        // get new states reachable from a given state
        expand:'a -> 'a seq;
        // state is the solution
        solved:'a -> bool
    }

    module AStar =
        let private addChild (parent:'a) (state:State<'a>) (child:'a) =
            let gval = state.g.Item(parent) + 1
            let g' = state.g.Add(child, gval)
            let fval = gval + (state.hueristic child)
            let f' = state.f.Add(child, fval)
            let a' = PriorityQueue.push state.available child fval
            { state with g=g'; f=f'; available=a' }

        let rec shortest (state:State<'a>) =
            let cur,avail' = PriorityQueue.pop state.available
            if (state.solved cur) then state.g.Item(cur)
            else
                let v' = state.visited.Add(cur)
                let state' = { state with visited=v'; available=avail' }
                let children = 
                    state.expand cur
                    |> Seq.filter (fun i -> not (v'.Contains(i)))
                let state'' =
                    children
                    |> Seq.fold (addChild cur) state'
                shortest state''