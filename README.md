# AofC_2022

Learning F# by doing [AdventOfCode 2022](https://adventofcode.com/2022)

Previous:
* [2021](https://github.com/JWMB/AofC_2021)


##Autogenerated##
## [Day 1 - Calorie Counting](https://adventofcode.com/2022/day/1)
[Source](AofC_2022/D1.fs) | [Input](AofC_2022/D1.txt)  
### part1
```FSharp
let part1 input =
    let result = Array.max (sums input)
    result
```
Result (in `3`ms): `71934`
### part2
```FSharp
let part2 input =
    let sorted = (sums input) |> Array.sortDescending
    let result = sorted |> Array.take 3 |> Array.sum
    result
```
Result (in `1`ms): `211447`
## [Day 2 - Rock Paper Scissors](https://adventofcode.com/2022/day/2)
[Source](AofC_2022/D2.fs) | [Input](AofC_2022/D2.txt)  
### part1
```FSharp
let part1 input =
    let rows = parseRows input
    let results = rows |> Array.map (fun f -> getOutcome f[0] f[1])
    let sum = results |> Array.sum
    sum
```
Result (in `2`ms): `10624`
### part2
```FSharp
let part2 input =
    let rows = parseRows input
    let results = rows |> Array.map (fun f -> (chooseItem f[0] (f[1] - 1)) + 1 + getPoints (f[1] - 1))
    let sum = results |> Array.sum
    sum
```
Result (in `2`ms): `14060`
## [Day 3 - Rucksack Reorganization](https://adventofcode.com/2022/day/3)
[Source](AofC_2022/D3.fs) | [Input](AofC_2022/D3.txt)  
### part1
```FSharp
let part1 input =
    let rucksacks = parseRows input
    let compartementalized = rucksacks |> Array.map (fun f -> f |> Array.splitAt (f.Length / 2))
    let samePer = compartementalized |> Array.map (fun (a, b) -> [| a; b |] |> intersectMany)
    let sum = samePer |> flatten |> Array.sum
    sum
```
Result (in `7`ms): `7997`
### part2
```FSharp
let part2 input =
    let rucksacks = parseRows input
    let numGroups = rucksacks.Length / 3
    let groups = rucksacks |> Array.splitInto numGroups
    let samePer = groups |> Array.map intersectMany
    let sum = samePer |> flatten |> Array.sum
    sum
```
Result (in `2`ms): `2545`
