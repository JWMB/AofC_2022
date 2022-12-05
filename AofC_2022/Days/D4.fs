module D4

open Tools

let parseRow row = RxCurry.split "," row |> Array.map (fun f -> RxCurry.split "-" f |> Array.map int)

let isRangeWithin rangeInner rangeOuter = Array.head rangeInner >= Array.head rangeOuter && rangeInner[1] <= rangeOuter[1]
let isRangeOverlap rangeInner rangeOuter = 
    let startEnd = [| max (Array.head rangeInner) (Array.head rangeOuter); min rangeInner[1] rangeOuter[1] |]
    startEnd[0] <= startEnd[1]

let part1 input =
    let pairs = Parsing.parseRows input parseRow
    let numWithCompleteOverlap = pairs |> Array.filter (fun pair -> isRangeWithin pair[0] pair[1] || isRangeWithin pair[1] pair[0]) |> Array.length
    numWithCompleteOverlap
    
let part2 input =
    let pairs = Parsing.parseRows input parseRow
    let numWithPartialOverlap = pairs |> Array.filter (fun pair -> isRangeOverlap pair[0] pair[1]) |> Array.length
    numWithPartialOverlap
 