﻿module D3

open System.Text.RegularExpressions

let charToPriority c = if c > 'Z' then 1 + int(c - 'a') else 27 + int(c - 'A')

let parseRows (input: string) = 
    Regex.Split(input.Trim().Replace("\r", ""), @"\n") 
    |> Array.map (fun f -> f |> Seq.map charToPriority |> Seq.toArray)

let findIndex arr elem = arr |> Array.findIndex ((=) elem)
let flatten arr = arr |> Array.collect (fun f -> f)
let intersectMany (arr: int array array) =
    arr |> Array.map Set.ofArray |> Array.reduce (fun a b -> Set.intersect a b) |> Set.toArray

let part1 input =
    let rucksacks = parseRows input
    let compartementalized = rucksacks |> Array.map (fun f -> f |> Array.splitAt (f.Length / 2))
    let samePer = compartementalized |> Array.map (fun (a, b) -> [| a; b |] |> intersectMany)
    let sum = samePer |> flatten |> Array.sum
    sum
    
let part2 input =
    let rucksacks = parseRows input
    let numGroups = rucksacks.Length / 3
    let groups = rucksacks |> Array.splitInto numGroups
    let samePer = groups |> Array.map intersectMany
    let sum = samePer |> flatten |> Array.sum
    sum