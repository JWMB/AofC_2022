module D11

open Tools
open System.Text.RegularExpressions

let parseRow row = [| row |]

type Monkey = { Operation: int64 -> int64; Test: int64-> int64; Divisor: int64; }
type MonkeyState = { Items: int64 array; NumInspected: int64 }

let parseSection input = 
    let afterColon str = (RxCurry.split ":" str)[1]
    let subsections = input |> RxCurry.split @"\n\s\s(?=\S)"

    let parseOperation str =
        // Operation: new = old * 19
        let operationDef = (str |> RxCurry.split @"=")[1] |> RxCurry.split @"\s" |> Array.tail
        let arithmeticOperator = operationDef[1]
        let terms = (operationDef[0], operationDef[2])
        let getTerm term old =
            match term with
            | "old" -> old
            | _ as v -> int64 (int v)

        let op old = 
            let t1 = getTerm (fst terms) old
            let t2 = getTerm (snd terms) old
            match arithmeticOperator with
            | "*" -> t1 * t2
            | "+" -> t1 + t2
            | "-" -> t1 - t2
            | _ -> t1
        op

    let parseTest str =
        //divisible by 23
        //    If true: throw to monkey 2
        //    If false: throw to monkey 3
        let divisibleBy = int64 (int Regex.Match(str, @"divisible by (\d+)").Groups[1].Value)
        let branches = Regex.Matches(str, @"If (true|false).+(\d+)", RegexOptions.Multiline) |> Seq.map (fun m -> (m.Groups[1].Value = "true", int64 (int m.Groups[2].Value))) |> Map.ofSeq
        let f v = branches[v % divisibleBy = 0L]
        (f, divisibleBy)

    let testInfo = parseTest subsections[3]
    (
        {
            Items = afterColon subsections[1] |> RxCurry.splitTrimNoEmpty "," |> Array.map (fun f -> int64 (int f));
            NumInspected = 0L;
        },
        {
            Operation = parseOperation subsections[2];
            Test = fst testInfo;
            Divisor = snd testInfo;
        }
    )

let parseInput input = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.split "\n\n" |> Array.map parseSection

let run initialStates (monkeys: Monkey array) postProcess numRounds =
    let doWork postProcess monkey state =
        let workOnItem item = 
            monkey.Operation item |> postProcess
        let itemsAndTargets =
            state.Items |> Array.map (fun item ->
                let newLevel = workOnItem item
                let passToMonkeyIndex = monkey.Test newLevel
                (newLevel, int passToMonkeyIndex)
            )
        itemsAndTargets

    let execute doWork (states: MonkeyState array) monkeyIndex = 
        let itemsAndTargets = doWork monkeys[monkeyIndex] states[monkeyIndex]
        let result = states |> Array.mapi (fun i state -> 
                            let newItems =
                                if i = monkeyIndex then
                                    [||]
                                else
                                    let itemsToThis = itemsAndTargets |> Array.filter (fun (_, target) -> target = i) |> Array.map (fun (item, _) -> item)
                                    itemsToThis |> Array.append states[i].Items

                            { Items = newItems; NumInspected = state.NumInspected + (if i = monkeyIndex then (int64 itemsAndTargets.Length) else 0L); }
                        )
        result

    let doWorkX monkey state = doWork postProcess monkey state

    let doRound states =
        let afterRound = 
            [|0..((Array.length states) - 1)|]
            |> Array.fold (execute doWorkX) states
        afterRound

    let final = [|1..numRounds|] |> Array.fold (fun agg _ -> doRound agg) initialStates
    final
    
let topInspectionsProduct topN states =
    states |> Array.map (fun x -> x.NumInspected)
    |> Array.sortByDescending (fun f -> f) |> Array.take topN
    |> Array.reduce (fun a b -> a * b)


let part1 input =
    let parsed = parseInput input
    let states = parsed |> Array.map (fun (state, _) -> state)
    let monkeys = parsed |> Array.map (fun (_, monkey) -> monkey)

    let numRounds = 20
    let postProcess v = v / 3L
    let final = run states monkeys postProcess numRounds

    topInspectionsProduct 2 final
    
let part2 input =
    let parsed = parseInput input
    let states = parsed |> Array.map (fun (state, _) -> state)
    let monkeys = parsed |> Array.map (fun (_, monkey) -> monkey)

    let divisorProduct = monkeys |> Array.map (fun f -> f.Divisor) |> Array.reduce (fun a b -> a * b)

    let numRounds = 10000
    let postProcess v = v % divisorProduct
    let final = run states monkeys postProcess numRounds

    let result = topInspectionsProduct 2 final
    result
