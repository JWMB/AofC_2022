module D9

open Tools

type Vector2D = { x: int; y: int; } with
    static member empty = { x = 0; y = 0; }
    member this.mul scalar = { x = this.x * scalar; y = this.y * scalar; }
    member this.add pt = { x = this.x + pt.x; y = this.y + pt.y; }
    member this.sub pt = { x = this.x - pt.x; y = this.y - pt.y; }
    member this.maxAbs = max (abs this.x) (abs this.y)
    member this.withSideMaxLength l =
        let normalize v = if v = 0 then 0 else v / abs v
        { x = l * (normalize this.x); y = l * (normalize this.y); }

type Rect = { topLeft: Vector2D; size: Vector2D } with
    static member empty = { topLeft = Vector2D.empty; size = Vector2D.empty;  }
    member this.expand pt = 
        let expand (value, length) newVal =
            if newVal < value then (newVal, (value - newVal) + length) 
            elif newVal > (value + length) then (value, newVal - value)
            else (value, length)
        let leftWidth = expand (this.topLeft.x, this.size.x) pt.x
        let topHeight = expand (this.topLeft.y, this.size.y) pt.y
        { topLeft = { x = fst leftWidth; y = fst topHeight}; size = { x = snd leftWidth; y = snd topHeight; }}

let parseRow row =
    let itemsToCmd (arr: string array) = 
        let move = match arr[0] with
        | "R" -> { x = 1; y = 0; }
        | "L" -> { x = -1; y = 0; }
        | "U" -> { x = 0; y = -1; }
        | "D" -> { x = 0; y = 1; }
        | _ -> { x = 0; y = 0; }
        (move, int arr[1])

    row |> RxCurry.split "\s" |> itemsToCmd

type State = { head: Vector2D; tail: Vector2D; }

let getPath headMoves initialPosition =
    let positions = { head = initialPosition; tail = initialPosition }

    let moveSnake current headMove = 
        let newHead = current.head.add headMove
        let diff = newHead.sub current.tail
        let tailMove =
            let withSide1 = diff.withSideMaxLength 1
            if withSide1 = diff then // close enough
                {x=0;y=0;}
            elif diff.x * diff.y = 0 then 
                if diff.x = 0 && diff.y = 0 then {x=0;y=0;}
                else withSide1
            else
                withSide1
        let newTail = current.tail.add tailMove
        { head = newHead; tail = newTail }

    let perInstruction lst curr = 
        let newState = moveSnake (lst |> List.last) curr
        [newState] |> List.append lst

    let folded = headMoves |> List.fold perInstruction [positions]
    folded

let visualize (lst: Vector2D list) =
    let size = lst |> List.fold (fun (agg: Rect) curr -> agg.expand curr) Rect.empty
    size

let part1 input =
    let instructions = Parsing.parseRows input parseRow

    let allHeadMoves =
        let step lst (pt, len) = ([1..len] |> List.map (fun f -> pt)) |> List.append lst 
        instructions |> Array.fold step []

    let trail = getPath allHeadMoves Vector2D.empty

    let allTails = trail |> List.map (fun f -> f.tail)
    let result = allTails |> List.distinct |> List.length

    result
    
let part2 input =
    let instructions = Parsing.parseRows input parseRow

    let headMovesToTailPositions headMoves =
        let trail = getPath headMoves Vector2D.empty
        trail |> List.map (fun f -> f.tail) |> List.tail // never any movement first step

    let absoluteToRelative (lst: Vector2D list) =
        lst |> List.windowed 2 |> List.map (fun x -> x[1].sub x[0])

    let relativeToAbsolute (lst: Vector2D list) =
        lst |> List.fold (fun agg curr -> 
            let newPos = curr.add (agg |> List.last)
            [newPos] |> List.append agg
            ) [Vector2D.empty]

    let allHeadMoves =
        let step lst (pt, len) = ([1..len] |> List.map (fun f -> pt)) |> List.append lst 
        instructions |> Array.fold step []

    let finalMoves = [1..9] |> List.fold (fun agg _ -> headMovesToTailPositions agg |> absoluteToRelative) allHeadMoves

    let absolute = relativeToAbsolute finalMoves
    //let rect = visualize absolute

    let result = absolute |> List.distinct |> List.length

    result
