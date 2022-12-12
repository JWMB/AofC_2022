module D12

open Tools
open Tools.Geometry

let parseRow (row: string) = row |> Seq.toArray |> Array.map (fun f -> char f)

let directions = [|{x=1;y=0}; {x=0;y=1}; {x= -1;y=0}; {x=0;y= -1}|]
//let getNeighbors (toPt: Vector2D) = directions |> Array.map (fun pt -> pt.add toPt)

type Grid = { Rect: Rect; Data: int array array; } with
    static member Create (rows: int array array) =
        let rect = Rect.getBoundingRect [| Vector2D.empty; { x = rows[0].Length; y = rows.Length }|]
        { Rect = rect; Data = rows }
    member this.isValid pt = pt.x >= 0 && pt.y >= 0 && pt.x < this.Rect.right && pt.y < this.Rect.bottom
    member this.getNeighbors toPt = directions |> Array.map (fun pt -> pt.add toPt) |> Array.filter (fun pt -> this.isValid pt)
    member this.getAt pt = this.Data[pt.y][pt.x]


let findChar rows char = 
    rows |> Array.indexed |> Array.map (fun (y, row) -> 
        let inRow = row |> Array.tryFindIndex (fun f -> f = char)
        (inRow, y)
        ) |> Array.filter (fun (x, y) -> x.IsSome) |> Array.map (fun (x, y) -> { x = x.Value; y = y; })

let parse input = 
    let rows = Parsing.parseRows input parseRow

    let startPos = findChar rows 'S' |> Array.head
    let finalPos = findChar rows 'E' |> Array.head

    let rows = Parsing.parseRows (input.Replace("S", "a").Replace("E", "z"))  parseRow
    (startPos, finalPos, Grid.Create (rows |> Array.map (fun r -> r |> Array.map int)))

let getPossible (grid: Grid) alreadyVisited currentPos =
        let currentVal = grid.getAt currentPos
        let notVisited = grid.getNeighbors currentPos |> Array.except alreadyVisited
        let withValues = notVisited |> Array.map (fun pt -> (pt, grid.getAt pt))

        let possible = withValues |> Array.filter (fun (_, v) -> v <= (currentVal + 1)) |> Array.map (fun (pt, _) -> pt)
        possible

let findPaths grid startPos finalPos =
    let costGrid = [|0..grid.Rect.height|] |> Array.map (fun f -> [|0..grid.Rect.width|] |> Array.map (fun z -> 99999))

    let rec loop pos path = seq {
        let possible = getPossible grid path pos
        if possible |> Array.contains finalPos then
            yield (path |> Array.append [|finalPos|])
        elif possible.Length > 0 then
            let currentCost = path.Length
            for pt in possible do
                let prevCost = costGrid[pt.y][pt.x]
                if currentCost < prevCost then
                    costGrid[pt.y][pt.x] <- currentCost
                    yield! (loop pt (path |> Array.append [|pt|]))
        }
    loop startPos [|startPos|]

let part1 input =
    let (startPos, finalPos, grid) = parse input        

    let paths = findPaths grid startPos finalPos
    let shortest = paths |> Seq.sortBy (fun f -> f.Length) |> Seq.head

    let result = shortest.Length - 1
    result
    
let part2 input =
    let (startPos, finalPos, grid) = parse input  

    let findChar (rows: char array array) char = 
        rows |> Array.indexed |> Array.map (fun (y, row) -> 
            let found = row |> Array.indexed |> Array.filter (fun (_, f) -> f = char) |> Array.map (fun (x, _) -> { x = x; y = y; })
            found
            ) |> Array.reduce Array.append

    let startPositions = findChar (Parsing.parseRows input parseRow) 'a' |> Array.append [|startPos|]

    // TODO: reverse, go from final to any 'a'

    let excludeInnerPatches =
        // performance - get continuous areas of 'a', change all internal except edges to very high value (= don't try that path)
        let surrounded = startPositions 
                            |> Array.filter (fun p -> 
                                let hasNonSameNeighbor = grid.getNeighbors p |> Array.map (fun n -> startPositions |> Array.contains n) |> Array.contains false
                                if hasNonSameNeighbor then false else true
                                )

        let startPositionsEdges = startPositions |> Array.except surrounded
        for pt in surrounded do
            grid.Data[pt.y][pt.x] <- 9999
        startPositionsEdges

    let startPositions = excludeInnerPatches

    let ooo = startPositions |> Array.map (fun p ->
            let paths = (findPaths grid p finalPos) |> Seq.toArray |> Array.filter (fun f -> f.Length > 0)
            if paths.Length = 0 then
                999999
            else
                let shortest = paths |> Seq.sortBy (fun f -> f.Length) |> Seq.head
                let result = shortest.Length - 1
                result
    )

    let result = ooo |> Array.sort |> Array.head
    result
