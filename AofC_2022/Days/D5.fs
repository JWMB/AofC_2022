module D5

open Tools

type Instruction = { Count: int; From: int; To: int; } 
    with
        static member FromArray (f: int array) = { Count = f[0]; From = f[1] - 1; To = f[2] - 1; }

        static member FromString row = row |> RxCurry.matches @"\d+" |> Seq.map (fun f -> int f.Value) |> Seq.toArray |> Instruction.FromArray

        static member Execute reverse instruction stacks =
            let liftOp = if reverse then Array.rev else Array.map (fun f -> f)
            let stackByIndex = stacks |> Array.mapi (fun i f -> i, f) |> Map.ofSeq // TODO: Array.indexed instead?
            let modified = stackByIndex |> Map.map (fun i f -> 
                if i = instruction.From then f |> Array.skip instruction.Count
                elif i = instruction.To then f |> Array.append (stackByIndex[instruction.From] |> Array.take instruction.Count |> liftOp)
                else f
            )
            let asArray = modified |> Map.toArray |> Array.sortBy (fun (i, _) -> i) |> Array.map (fun (_, f) -> f)
            asArray

type ParsedInput = { Stacks: char array array; Instructions: Instruction array }
    with
        static member Parse input =
            let getCharsAtIndices indices (str: string) = indices |> Array.map (fun f -> str[f])

            let sections = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.split "\n\n"

            let topSection = sections[0] |> RxCurry.split "\n"
            let columnIndices = Array.last topSection |> RxCurry.matches "\d+" |> Seq.map (fun f -> f.Index) |> Seq.toArray
            let stackRows = topSection |> Array.map (getCharsAtIndices columnIndices) |> ArrayEx.exceptLast
            let stacks = columnIndices |> Array.mapi (fun i _ -> stackRows |> Array.map (fun f -> f[i]) |> Array.filter (fun f -> f <> ' '))

            let instructions = sections[1] |> RxCurry.split "\n" |> Array.map Instruction.FromString
            { Stacks = stacks; Instructions = instructions }

let foldWithFunction func instructions stacks =
    instructions |> Array.fold (fun agg curr -> func curr agg) stacks

let part1 input =
    let data = ParsedInput.Parse input

    let func = Instruction.Execute true
    let modifiedStacks = foldWithFunction func data.Instructions data.Stacks

    let result = modifiedStacks |> Array.map (fun f -> f[0]) |> Array.map string |> String.concat ""
    result
    
let part2 input =
    let data = ParsedInput.Parse input

    let func = Instruction.Execute false
    let modifiedStacks = foldWithFunction func data.Instructions data.Stacks

    let result = modifiedStacks |> Array.map (fun f -> f[0]) |> Array.map string |> String.concat ""
    result
