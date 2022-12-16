module D15

open Tools
open Tools.Geometry
open System.Text.RegularExpressions

let getManhattan pt1 pt2 = abs (pt1.x - pt2.x) + abs (pt1.y - pt2.y)

type Sensor = { Location: Vector2D; Beacon: Vector2D } with
    member this.manhattanDist = getManhattan this.Beacon this.Location
    member this.isInRange pt = (getManhattan pt this.Location) <= this.manhattanDist

type Grid = { Rect: Rect; } with
    static member create allPoints =
        let rect = Rect.getBoundingRect allPoints
        { Rect = rect; }

let parseRow row = 
    let pattern variable = @$"({variable}=(?<{variable}>-?\d+))"
    let points = Regex.Matches(row, $"""{pattern "x"}, {pattern "y"}""") |> Seq.map (fun m -> { x = int m.Groups["x"].Value; y = int m.Groups["y"].Value }) |> Seq.toArray
    { Location = points[0]; Beacon = points[1] }

let getInRangePointsXbyY pt manhattan =
    let inRange =
        [|-manhattan..manhattan|] |> Array.map (fun y -> 
            let width = manhattan - (abs y)
            (y + pt.y, [|-width..width|] |> Array.map (fun x -> x + pt.x) |> Set.ofArray )
        )
    inRange |> Map.ofSeq

let getInRangeFromSensor sensor =
    getInRangePointsXbyY sensor.Location sensor.manhattanDist

let removeImpossibles impossibleXByY coveredXbyY =
    let commonY = Set.intersect (Map.keys impossibleXByY |> Set.ofSeq) (Map.keys coveredXbyY |> Set.ofSeq)
    let result = impossibleXByY |> Map.map (fun y xs -> 
            if commonY |> Set.contains y then Set.difference xs coveredXbyY[y]
            else xs
        )
    result

let removeInRange impossibleXByY sensor =
    let inRange = getInRangeFromSensor sensor
    inRange|> removeImpossibles impossibleXByY

let visualize grid (impossibleXByY: Map<int, Set<int>>) =
    [|grid.Rect.top..grid.Rect.bottom|] |> Array.map (fun y ->
        let line = [|grid.Rect.left..grid.Rect.right|] |> Array.map (fun x -> if impossibleXByY[y] |> Set.contains x then "." else "#") |> String.concat ""
        $"{y.ToString().PadLeft(2)} {line}"
    ) |> String.concat "\n"
    
let isInAnyRange ranges value =
    let isInRange x rng = x >= fst rng && x <= snd rng
    ranges |> Array.exists (isInRange value)

let visualizeRow (rect: Rect) ranges =
    //let isInRange x rng = x >= fst rng && x <= snd rng
    //[|rect.left..rect.right|] |> Array.map (fun x -> 
    //                            let sss = ranges |> Array.exists (isInRange x)
    //                            if sss then "#" else "."
    [|rect.left..rect.right|] |> Array.map (isInAnyRange ranges) |> Array.map (fun v -> if v then "#" else ".") |> String.concat ""

let rec combine numItems list = 
    match numItems, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combine (k-1) xs) @ combine k xs

let getSensorCoverage sensors y =
    //let allLocations = sensors |> Array.map (fun f -> [|f.Location;f.Beacon;|]) |> Array.reduce Array.append
    //let rect = Rect.getBoundingRect allLocations
    let possibleSensors = sensors |> Array.filter (fun sensor -> 
        let yDiff = y - sensor.Location.y
        sensor.manhattanDist - (abs yDiff) >= 0)

    let xRangePerSensor = possibleSensors |> Array.map (fun sensor ->
            let yDiff = abs (sensor.Location.y - y)
            let widthAtY = sensor.manhattanDist - yDiff
            (sensor.Location.x - widthAtY, sensor.Location.x + widthAtY)
        )
    let rec mergeRanges ranges =
        //let getMerged r1 r2 = //(r1, r2) =
        //    let isNonOverlapping = ((snd r1) < (fst r2)) && ((snd r1) < (fst r2))
        //    if isNonOverlapping then None
        //    else Some((min (fst r1) (fst r2), max (snd r1) (snd r2)))
        let getMerged (min1, max1) (min2, max2) =
            let isNonOverlapping = if min1 <= min2 then max1 < min2 else max2 < min1
            if isNonOverlapping then None
            else Some((min min1 min2, max max1 max2))

        if ranges |> Array.length <= 1 then ranges
        else
            // test N with N+1.. if any merge happened, remove N and M and put merged first, the restart
            let foundCombined = combine 2 (ranges |> Array.toList)
                                    |> List.toSeq |> Seq.map (fun f -> 
                                        let m = getMerged f[0] f[1]
                                        if m.IsSome then Some({| Merged = m.Value; Source = [|f[0]; f[1]|]; |})
                                        else None
                                        )
                                    |> Seq.tryFind (fun f -> f.IsSome)
            if foundCombined.IsSome then
                let newRanges = ranges |> Array.except foundCombined.Value.Value.Source |> Array.append [|foundCombined.Value.Value.Merged|]
                mergeRanges newRanges
            else ranges

    let merged = mergeRanges xRangePerSensor
    merged

let getBoundingRect sensors =
    let allLocations = sensors |> Array.map (fun f -> [|f.Location;f.Beacon;|]) |> Array.reduce Array.append
    let sensorExtra = sensors |> Array.map (fun f -> [|-1;1|] |> Array.map (fun d -> {x = d; y = d}.mul f.manhattanDist) |> Array.map (fun p -> p.add f.Location)) |> Array.reduce Array.append
    let allLocations = allLocations |> Array.append sensorExtra
    let rect = Rect.getBoundingRect allLocations
    rect

let part1WithY input y =
    let sensors = Parsing.parseRows input parseRow

    let rowOfInterest = getSensorCoverage sensors y
    let total = rowOfInterest |> Array.map (fun (a, b) -> b - a) |> Array.sum

    //let impossibleXByY = [|grid.Rect.top..grid.Rect.bottom|] |> Array.map (fun y -> (y, [|grid.Rect.left..grid.Rect.right|] |> Set.ofArray)) |> Map.ofSeq
    //let before = visualize grid impossibleXByY
    //let after1 = visualize grid (removeInRange impossibleXByY sensors[6])
    //let remainingImpossibleXByY = sensors |> Array.fold removeInRange impossibleXByY
    //let afterAll = visualize grid remainingImpossibleXByY

    //let result = grid.Rect.width - remainingImpossibleXByY[y].Count
    let result = total
    result

let part1 input =
    part1WithY input 2000000
    
let part2WithMinMax input min max =
    let sensors = Parsing.parseRows input parseRow
    
    //let rangesOn11 = getSensorCoverage sensors 11


    let rangesPerY = { min..max } |> Seq.map (getSensorCoverage sensors)

    //let rect = getBoundingRect sensors
    //let visualized = rangesPerY |> Seq.map (visualizeRow rect) |> String.concat "\n"

    let foundOption = rangesPerY |> Seq.indexed 
                        |> Seq.map (fun (i, ranges) ->
                            let ofInterest = ranges |> Array.filter (fun (xmin, xmax) -> xmin > min || xmax < max)
                            if ofInterest.Length > 0 then
                                if ofInterest.Length > 2 then None
                                else
                                    //{(-1628590, 2889604)} {(2889606, 4238366)}
                                    //{(15, 25)}	{(-3, 13)} -> { x = 14; y = 11; }
                                    let x =
                                        if (fst ofInterest[0]) < (fst ofInterest[1]) then (snd ofInterest[0]) + 1 
                                        else (snd ofInterest[1]) + 1
                                    Some({ x = x; y = i + min })
                            else 
                                None
                            )
                        |> Seq.find (fun f -> f.IsSome)
    let found = foundOption.Value
    // { x = 2889605; y = 3398893; } // 666405357 too low

    let result = found.x * 4000000 + found.y
    result

let part2 input =
    part2WithMinMax input 0 4000000