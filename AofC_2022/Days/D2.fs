module D2

open System.Text.RegularExpressions

open Tools

let zz = [|'A'; 'X'|] |> Array.map int
let rowToIndices row = row |> Array.mapi (fun i f -> f - zz[i])
let parseRow (row: string) = row.Trim() |> RxCurry.split " " |> Array.map char |> Array.map int |> rowToIndices

let getPoints x = (x + 1) * 3

let winnerMatrix = [|
    // dimension 0  is opponent
        [|0; 1; -1|] // Rock
        [|-1; 0; 1|] // Paper
        [|1; -1; 0|] // Scissors
    |]

let getOutcome p0 p1 = (winnerMatrix[p0][p1] |> getPoints) + p1 + 1

let findIndex arr elem = arr |> Array.findIndex ((=) elem)
let chooseItem p0 expectedOutcome = findIndex winnerMatrix[p0] expectedOutcome

let part1 input =
    let rows = Parsing.parseRows input parseRow
    let results = rows |> Array.map (fun f -> getOutcome f[0] f[1])
    let sum = results |> Array.sum
    sum
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let results = rows |> Array.map (fun f -> (chooseItem f[0] (f[1] - 1)) + 1 + getPoints (f[1] - 1))
    let sum = results |> Array.sum
    sum
