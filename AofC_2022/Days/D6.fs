module D6

open Tools


let findFirstNonRepeatingStringOfLength length agg curr =
    if (snd agg) |> String.length = length then agg // already found solution - no further modifications
    else
        let foundIndex = (snd agg) |> Seq.tryFindIndex (fun f -> (char (snd curr)) = f)
        let keepFromIndex = match foundIndex with
                            | Some i -> i + 1
                            | None -> 0
        if (snd agg) |> Seq.length = 0 then curr // initial condition
        else ((fst agg) + keepFromIndex, StringEx.join ((snd agg)[keepFromIndex..]) (snd curr))

let getMarkerEndIndex x markerLength = 
        if (snd x) |> String.length = markerLength then (fst x) + markerLength
        else -1

let stringToIndexedTuples str = str|> Seq.mapi (fun i f -> (i, string f))

let part1 input =
    let markerLength = 4
    let x = Parsing.cleanWithTrimEmptyLines input
            |> stringToIndexedTuples
            |> Seq.fold (findFirstNonRepeatingStringOfLength markerLength) (0, "")

    let result = getMarkerEndIndex x markerLength
    result
    
let part2 input =
    let markerLength = 14
    let x = Parsing.cleanWithTrimEmptyLines input
            |> stringToIndexedTuples
            |> Seq.fold (findFirstNonRepeatingStringOfLength markerLength) (0, "")

    let result = getMarkerEndIndex x markerLength
    result
