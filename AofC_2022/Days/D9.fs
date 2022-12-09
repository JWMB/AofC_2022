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
    member this.left = this.topLeft.x
    member this.right = this.topLeft.x + this.size.x
    member this.top = this.topLeft.y
    member this.bottom = this.topLeft.y + this.size.y
    member this.width = this.size.x
    member this.height = this.size.y

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

let getTailPath headPositions (initialPosition: Vector2D) =
    let moveSnake (currentTail: Vector2D) (newHead: Vector2D) = 
        let diff = newHead.sub currentTail
        let tailMove =
            let withSide1 = diff.withSideMaxLength 1
            if withSide1 = diff then // close enough
                {x=0;y=0;}
            elif diff.x * diff.y = 0 then 
                if diff.x = 0 && diff.y = 0 then {x=0;y=0;}
                else withSide1
            else
                withSide1
        let newTail = currentTail.add tailMove
        newTail

    let perInstruction lst (curr: Vector2D) =
        let newTail = moveSnake (lst |> Array.last) curr
        [|newTail|] |> Array.append lst

    let folded = headPositions |> Array.fold perInstruction [|initialPosition|]
    folded


let visualize (lst: Vector2D array) =
    let distinct = lst |> Array.distinct
    let rect = distinct |> Array.fold (fun (agg: Rect) curr -> agg.expand curr) Rect.empty

    // let hasItem pt = distinct |> Array.contains pt
    // let image = Gif.createImageWithXYFunc rect.width rect.height (fun x y -> if hasItem {x = x + rect.left; y = y + rect.top;} then 'x' else ' ')

    use image = distinct |> Array.toSeq |> Seq.map (fun pt -> (pt.x - rect.left, pt.y - rect.top, 'x')) |> Gif.createImageWithPixelSeq (rect.width+1) (rect.height+1)
    Gif.saveAsGif "test.gif" image

let absoluteToRelative (lst: Vector2D array) =
    lst |> Array.windowed 2 |> Array.map (fun x -> x[1].sub x[0])

let relativeToAbsolute (lst: Vector2D array) =
    lst |> Array.fold (fun agg curr -> 
        let newPos = curr.add (agg |> Array.last)
        [|newPos|] |> Array.append agg
        ) [|Vector2D.empty|]

let instructionsToHeadPath instructions =
    let step lst (pt, len) = ([|1..len|] |> Array.map (fun f -> pt)) |> Array.append lst 
    instructions |> Array.fold step [||] |> relativeToAbsolute

let part1 input =
    let headPath = instructionsToHeadPath (Parsing.parseRows input parseRow)
    let tailPath = getTailPath headPath Vector2D.empty
    let result = tailPath |> Array.distinct |> Array.length
    result
    
let part2 input =
    let headPath = instructionsToHeadPath (Parsing.parseRows input parseRow)

    let headPathToTailPath headPositions =
        let tailPath = getTailPath headPositions Vector2D.empty
        tailPath |> Array.tail // never any movement first step

    let finalTailPath = [|1..9|] |> Array.fold (fun agg _ -> headPathToTailPath agg) headPath
    //visualize finalTailPath

    let result = finalTailPath |> Array.distinct |> Array.length
    result
