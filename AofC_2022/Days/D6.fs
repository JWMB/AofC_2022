module D6

open Tools


let findFirstNonRepeatingStringOfLength length (aggegateIndex, aggregateStr) (index, char) =
    if aggregateStr |> String.length = length then (aggegateIndex, aggregateStr) // already found solution - no further modifications
    else
        let foundIndex = aggregateStr |> Seq.tryFindIndex (fun f -> char = f)
        let keepFromIndex = match foundIndex with
                            | Some i -> i + 1
                            | None -> 0
        if aggregateStr.Length = 0 then (index, string char) // initial condition
        else (aggegateIndex + keepFromIndex, StringEx.join (aggregateStr[keepFromIndex..]) (string char))

let getMarkerEndIndex (index, str) markerLength = 
        if str |> String.length = markerLength then index + markerLength
        else -1

let part1 input =
    let markerLength = 4

    let x = Parsing.cleanWithTrimEmptyLines input
            |> Seq.indexed
            |> Seq.fold (findFirstNonRepeatingStringOfLength markerLength) (0, "")

    let result = getMarkerEndIndex x markerLength
    result
    
let part2 input =
    let markerLength = 14
    let x = Parsing.cleanWithTrimEmptyLines input
            |> Seq.indexed
            |> Seq.fold (findFirstNonRepeatingStringOfLength markerLength) (0, "")

    let result = getMarkerEndIndex x markerLength
    result
