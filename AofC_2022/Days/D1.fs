module D1

open Tools

let splitPerPerson (input: string) = 
    input |> Parsing.clean |> RxCurry.split "\n\n" |> Array.map (fun f -> f |> RxCurry.split "\n" |> Array.map int)

let sums (input: string) = input |> splitPerPerson |> Array.map(fun f -> Array.sum f)

let part1 input =
    let result = Array.max (sums input)
    result
    
let part2 input =
    let sorted = (sums input) |> Array.sortDescending
    let result = sorted |> Array.take 3 |> Array.sum
    result
