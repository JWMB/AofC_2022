module D6

open Tools

let strcat str1 str2 = String.concat "" [| str1; str2; |]

let findFirstNonRepeatingStringOfLength length agg curr =
    if (snd agg) |> String.length = length then agg
    else
        let foundIndex = (snd agg) |> Seq.tryFindIndex (fun f -> (char (snd curr)) = f)
        let keepFromIndex = match foundIndex with
                            | Some i -> i + 1
                            | None -> 0
        if (snd agg) |> Seq.length = 0 then curr
        else ((fst agg) + keepFromIndex, strcat ((snd agg)[keepFromIndex..]) (snd curr))

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
