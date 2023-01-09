module D17

open Tools
open Tools.Geometry

let parseRow row = [| row |]

let getRocks =
    let input = """
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
"""
    let parseSection input = 
        let parseRow y (input: string) =
            input |> Seq.indexed |> Seq.filter(fun (_, c) -> c = '#') |> Seq.map (fun (x, _) -> { x = x; y = y;}) |> Seq.toArray
        input |> RxCurry.split "\n" |> Array.mapi parseRow |> Array.reduce Array.append

    let rocks = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.splitTrimNoEmpty("\n\n") |> Array.map parseSection
    rocks

type Grid = { Rect: Rect; Rows: string array } with
    member this.IsFree pt =
        if pt.y < 0 then true
        elif pt.y >= this.Rect.bottom then false
        else this.Rows[pt.y][pt.x] <> '#'
    member this.Add points ptOffset =
        //this.Rows
        this
        //let addToGrid grid rock pt =
        ////rock |> Array.map (fun p -> )
        //grid 
    static member create = { Rect = Rect.empty; Rows = [||] } //[| [|1..width|] |> Array.map (fun _ -> "#") |> String.concat "" |]

// |.......|
// |#######|

let part1 (input: string) =
    let rocks = getRocks
    let jets = input
    let width = 7

    let endAtRock = 2022
    let getNextRock index = rocks[index % rocks.Length]
    let getNextJet index = { 
                                x = if jets[index % jets.Length] = '<' then -1 else 1;
                                y = 0;
                           }

    let isPossible (grid: Grid) rock pt =
        let rockWidth = rock |> Array.map (fun pt -> pt.x) |> Array.max
        if pt.x < 0 || rockWidth + pt.x > width then false
        else
            let hasOverlap = rock |> Array.exists (fun p -> grid.IsFree (p.add pt))
            hasOverlap

    let fall grid rock index =
        
        let pt = { x = 2; y = 0; }
        let rec loop pt index =
            let tryMove (pt: Vector2D) (move: Vector2D) = if isPossible grid rock (pt.add move) then pt.add move else pt

            let pt = tryMove pt (getNextJet index)
            let newPt = tryMove pt { x = 0; y = 1; }
            if pt = newPt then (pt, index)
            else loop pt (index + 1)

        let final = loop pt index
        final

    let rec loop grid rockCount step =
        let rock = getNextRock rockCount
        let (pt, index) = fall grid rock step
        let newGrid = grid.Add rock pt
        if rockCount = endAtRock then newGrid
        else loop grid (rockCount + 1) index

    let grid = Grid.create
    let aaa = loop grid 0 0

    let result = aaa.Rect.height
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
