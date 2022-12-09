module D8

open Tools

let parseRow row = row |> Seq.map int |> Seq.map (fun f -> f - 48) |> Seq.toArray
type Vector2D = { x: int; y: int; }

let add pt1 pt2 = { x = pt1.x + pt2.x; y = pt1.y + pt2.y }

type Matrix<'T> = { data: 'T array array } with
    member this.getAt pt = this.data[pt.y][pt.x]
    member this.isInside pt = pt.x >= 0 && pt.y >= 0 && pt.x < this.data[0].Length && pt.y < this.data.Length
    member this.size = { x = this.data[0].Length; y = this.data.Length; }

let rec lineOfSight (matrix: Matrix<'T>) startPoint step = seq { 
    if matrix.isInside startPoint then yield startPoint
    let ptNext = add startPoint step
    if matrix.isInside ptNext then yield! lineOfSight matrix ptNext step
}

let addToList lst item = [item] |> List.append lst

let part1 input =
    let matrix = { data = Parsing.parseRows input parseRow }

    let getMaxAndVisible pt step = lineOfSight matrix pt step 
                                |> Seq.fold (fun (currentMax, lst) curr -> 
                                    let height = matrix.getAt curr
                                    if height > currentMax then (height, [curr] |> List.append lst) else (currentMax, lst))
                                    (0, [])

    let getVisible pt step = snd (getMaxAndVisible pt step)

    let viewpoints numSteps otherside =
        [1..numSteps] |> List.map (fun x -> 
            [ (0, [0;1]); (otherside, [0;-1])] |> List.map (fun (y, dir) -> ([x;y], dir))
        ) |> List.reduce List.append
    
    let forX = viewpoints (matrix.size.x - 2) (matrix.size.y-1)
    let forY = viewpoints (matrix.size.y - 2) (matrix.size.x-1) |> List.map (fun (pt, dir) -> (pt |> List.rev, dir |> List.rev))

    let lstToPt (lst: int list) = { x = lst[0]; y = lst[1]; }
    let allViewpoints = forX |> List.append forY |> List.map (fun (pt, dir) -> (lstToPt pt, lstToPt dir))

    let visibleFromViewpoints = allViewpoints |> List.map (fun (pt, step) -> getVisible pt step) |> List.reduce List.append |> List.distinct

    let exceptEdges = visibleFromViewpoints |> List.filter (fun pt -> pt.x > 0 && pt.y > 0 && pt.x < (matrix.size.x-1) && pt.y < (matrix.size.y-1))

    let result = exceptEdges.Length + matrix.size.x * 2 + matrix.size.y * 2 - 4
    result
    
let part2 input =
    let matrix = { data = Parsing.parseRows input parseRow }
    let directions = [{x=1;y=0}; {x= -1;y=0}; {x=0;y=1}; {x=0;y= -1}]
    let viewpoints = [0..matrix.size.x-1] |> List.map (fun x -> [0..matrix.size.y-1] |> List.map (fun y -> {x=x;y=y;})) |> List.reduce List.append

    let folder threshold (currentMax, lst) curr = 
        let newMax = max (matrix.getAt curr) currentMax
        if newMax >= threshold then 
            if currentMax < 99 then (99, addToList lst curr)
            else (currentMax,lst)
        else
            (newMax, addToList lst curr)

    let getClearingDistance pt direction threshold = lineOfSight matrix pt direction |> Seq.fold (folder threshold) (0, [])
    let getClearingDistanceX pt direction = snd (getClearingDistance (add pt direction) direction (matrix.getAt pt))
    
    let getViewLengths pt = directions |> List.map (fun dir -> getClearingDistanceX pt dir) |> List.map (fun f -> f.Length)

    let visibleFromViewpoints = viewpoints |> List.map (fun pt -> (getViewLengths pt))
    let mul lst = lst |> List.reduce (fun a b -> a * b)
    let scores = visibleFromViewpoints |> List.map mul

    let result = scores |> List.max
    result
