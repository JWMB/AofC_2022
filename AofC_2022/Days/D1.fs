module D1

open System.Text.RegularExpressions

let splitPerPerson (input: string) = Regex.Split(input.Trim().Replace("\r", ""), @"\n\n") |> Array.map (fun f -> Regex.Split(f.Trim(), "\n") |> Array.map int)
let sums (input: string) = input |> splitPerPerson |> Array.map(fun f -> Array.sum f)

let part1 input =
    let result = Array.max (sums input)
    result
    
let part2 input =
    let sorted = (sums input) |> Array.sortDescending
    let result = sorted |> Array.take 3 |> Array.sum
    result
