module D14

open Tools
open Tools.Geometry

let parseRow row =
    let pairs = row |> RxCurry.splitTrimNoEmpty "->" |> Array.map (fun item -> item |> RxCurry.splitTrimNoEmpty "," |> Array.map (fun v -> int v))
    let points = pairs |> Array.map (fun p -> { x = p[0]; y = p[1] })

    let createIntermediatePoints ((a: Vector2D), (b: Vector2D)) =
        let diff = b.sub a
        let numSteps = diff.maxAbs
        let step = diff.withSideMaxLength 1
        [|0..numSteps|] |> Array.map (fun i -> a.add (step.mul i))

    let linePoints = points |> Array.pairwise |> Array.map createIntermediatePoints |> Array.reduce Array.append |> Array.distinct

    linePoints

type Grid = { Rect: Rect; Data: string array array; Source: Vector2D; Rocks: Vector2D array } with
    static member create rockPoints source =
        let withSource = rockPoints |> Array.append [| source |]
        let rect = Rect.getBoundingRect withSource

        let rockPoints = rockPoints |> Array.map (fun p -> p.sub rect.topLeft)
        let source = source.sub rect.topLeft
        let rect = rect.normalize

        let grid = [|rect.top..rect.bottom|] |> Array.map (fun y ->
                [|rect.left..rect.right|] |> Array.map (fun x ->
                    let pt = { x = x; y = y; }
                    if rockPoints |> Array.contains pt then "#"
                    else "."
                )
            )

        { Rect = rect; Data = grid; Source = source; Rocks = rockPoints; }

    member this.render = 
        (this.Data |> Array.map (fun r -> String.concat "" r)) |> String.concat "\n"
    //member this.normalize =
    //    let tl = this.Rect.topLeft
    //    { Rect = this.Rect.normalize; Source = this.Source.sub tl; Data = }

let rec fall grid pt =
    let getAt p = grid.Data[p.y][p.x]
    let setAt p v = grid.Data[p.y][p.x] <- v
    let isInside p = grid.Rect.contains p

    let tryCoordinates = [| { x = 0; y = 1;}; { x = -1; y = 1;};{ x = 1; y = 1;}; |] |> Array.toSeq
    let foundEmptyPoint = tryCoordinates |> Seq.tryFind (fun p ->
                                let pn = p.add pt
                                if isInside pn then getAt pn = "." else true)
    if foundEmptyPoint.IsSome then
        let np = foundEmptyPoint.Value.add pt
        if isInside np then
            fall grid np
        else
            None
    else
        if getAt pt = "o" then None
        else
            setAt pt "o"
            Some pt

let part1 input =
    let linePoints = Parsing.parseRows input parseRow |> Array.reduce Array.append

    let grid = Grid.create linePoints { x = 500; y = 0; }

    let rec untilOutsize cnt =
        let foundRestingPlace = fall grid grid.Source
        if foundRestingPlace.IsNone then cnt
        else untilOutsize (cnt + 1)

    let numDrops = untilOutsize 0
    numDrops
    
let part2 input =
    let linePoints = Parsing.parseRows input parseRow |> Array.reduce Array.append
    let grid = Grid.create linePoints { x = 500; y = 0; }

    let newHeight = grid.Rect.bottom + 2 // for bedrock

    // expand grid so that width = height and source is in middle of x axis
    let newWidth = 2 * newHeight //bah, just make it big enough
    let newLeft = min (grid.Source.x - newWidth) grid.Rect.left
    let newRight = max (grid.Source.x + newWidth) grid.Rect.right

    let bedrock = [|newLeft..newRight|] |> Array.map(fun x -> { x = x; y = newHeight; })
    
    let grid = Grid.create (grid.Rocks |> Array.append bedrock) grid.Source

    //let before = grid.render

    let rec untilOutsize cnt =
        let foundRestingPlace = fall grid grid.Source
        if foundRestingPlace.IsNone then cnt
        else untilOutsize (cnt + 1)
        
    let numDrops = untilOutsize 0

    //let after = grid.render

    numDrops
