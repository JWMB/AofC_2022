# AofC_2022

Learning F# by doing [AdventOfCode 2022](https://adventofcode.com/2022)

Previous:
* [2021](https://github.com/JWMB/AofC_2021)


##Autogenerated##
## [Day 1 - Calorie Counting](https://adventofcode.com/2022/day/1)
[Source](/AofC_2022/Days/D1.fs) | [Input](/AofC_2022/Days/D1.txt)  
### part1
```FSharp
let part1 input =
    let result = Array.max (sums input)
    result
```
Result (in `4`ms): `71934`
### part2
```FSharp
let part2 input =
    let sorted = (sums input) |> Array.sortDescending
    let result = sorted |> Array.take 3 |> Array.sum
    result
```
Result (in `3`ms): `211447`
## [Day 2 - Rock Paper Scissors](https://adventofcode.com/2022/day/2)
[Source](/AofC_2022/Days/D2.fs) | [Input](/AofC_2022/Days/D2.txt)  
### part1
```FSharp
let part1 input =
    let rows = Parsing.parseRows input parseRow
    let results = rows |> Array.map (fun f -> getOutcome f[0] f[1])
    let sum = results |> Array.sum
    sum
```
Result (in `4`ms): `10624`
### part2
```FSharp
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let results = rows |> Array.map (fun f -> (chooseItem f[0] (f[1] - 1)) + 1 + getPoints (f[1] - 1))
    let sum = results |> Array.sum
    sum
```
Result (in `4`ms): `14060`
## [Day 3 - Rucksack Reorganization](https://adventofcode.com/2022/day/3)
[Source](/AofC_2022/Days/D3.fs) | [Input](/AofC_2022/Days/D3.txt)  
### part1
```FSharp
let part1 input =
    let rucksacks = Parsing.parseRows input parseRow
    let compartmentalized = rucksacks |> Array.map (fun f -> f |> Array.splitAt (f.Length / 2))
    let samePer = compartmentalized |> Array.map (fun (a, b) -> [| a; b |] |> intersectMany)
    let sum = samePer |> flatten |> Array.sum
    sum
```
Result (in `10`ms): `7997`
### part2
```FSharp
let part2 input =
    let rucksacks = Parsing.parseRows input parseRow
    let numGroups = rucksacks.Length / 3
    let groups = rucksacks |> Array.splitInto numGroups
    let samePer = groups |> Array.map intersectMany
    let sum = samePer |> flatten |> Array.sum
    sum
```
Result (in `4`ms): `2545`
## [Day 4 - Camp Cleanup](https://adventofcode.com/2022/day/4)
[Source](/AofC_2022/Days/D4.fs) | [Input](/AofC_2022/Days/D4.txt)  
### part1
```FSharp
let part1 input =
    let pairs = Parsing.parseRows input parseRow
    let numWithCompleteOverlap = pairs |> Array.filter (fun pair -> isRangeWithin pair[0] pair[1] || isRangeWithin pair[1] pair[0]) |> Array.length
    numWithCompleteOverlap
```
Result (in `8`ms): `450`
### part2
```FSharp
let part2 input =
    let pairs = Parsing.parseRows input parseRow
    let numWithPartialOverlap = pairs |> Array.filter (fun pair -> isRangeOverlap pair[0] pair[1]) |> Array.length
    numWithPartialOverlap
```
Result (in `4`ms): `837`
## [Day 5 - Supply Stacks](https://adventofcode.com/2022/day/5)
[Source](/AofC_2022/Days/D5.fs) | [Input](/AofC_2022/Days/D5.txt)  
### part1
```FSharp
let part1 input =
    let data = ParsedInput.Parse input

    let reverse = true
    let modifiedStacks = data.Instructions |> Array.fold (fun agg curr -> curr.Apply reverse agg) data.Stacks

    let result = modifiedStacks |> Array.map (fun f -> f[0]) |> Array.map string |> String.concat ""
    result
```
Result (in `17`ms): `SBPQRSCDF`
### part2
```FSharp
let part2 input =
    let data = ParsedInput.Parse input

    let reverse = false
    let modifiedStacks = data.Instructions |> Array.fold (fun agg curr -> curr.Apply reverse agg) data.Stacks

    let result = modifiedStacks |> Array.map (fun f -> f[0]) |> Array.map string |> String.concat ""
    result
```
Result (in `5`ms): `RGLVRCQSB`
