module PriorityQueue

    type Queue<'a when 'a : comparison> = 
        { items:Map<'a,int>; rank:'a -> int } 

    let init (rank:'a -> int) =
        { items=Map.empty; rank=rank }

    let push (q:Queue<'a>) (v:'a) (r:int) =
        { q with items=(q.items.Add(v, r)) }

    let pop (q:Queue<'a>) =
        let v,r =
            q.items
            |> Map.toList
            |> List.minBy (fun i -> snd i)
        let q' = { q with items=(q.items.Remove(v)) }
        (v, q')

    let empty (q:Queue<'a>) = q.items.IsEmpty
