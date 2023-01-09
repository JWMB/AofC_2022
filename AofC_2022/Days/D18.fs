module D18

open Tools
open Tools.Geometry

let parseRow row = 
    let cells = row |> RxCurry.split "," |> Array.map (fun s -> int s)
    { x = cells[0]; y = cells[1]; z = cells[2]; }

let part1 input =
    let cubes = Parsing.parseRows input parseRow
    let result = 0
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
