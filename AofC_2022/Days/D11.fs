module D11

open Tools
open System.Text.RegularExpressions

let parseRow row = [| row |]

//type MonkeyTest = { Execute: int -> bool; TrueTarget: int; FalseTarget: int }
type Monkey = { Operation: bigint -> bigint; Test: bigint -> bigint }
type MonkeyState = { Items: bigint array; NumInspected: bigint }

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
            | _ as v -> bigint (int v)

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
        let divisibleBy = bigint (int Regex.Match(str, @"divisible by (\d+)").Groups[1].Value)
        let branches = Regex.Matches(str, @"If (true|false).+(\d+)", RegexOptions.Multiline) |> Seq.map (fun m -> (m.Groups[1].Value = "true", bigint (int m.Groups[2].Value))) |> Map.ofSeq
        let f v = branches[v % divisibleBy = 0I]
        f
    (
        {
            Items = afterColon subsections[1] |> RxCurry.splitTrimNoEmpty "," |> Array.map (fun f -> bigint (int f));
            NumInspected = 0I;
        },
        {
            Operation = parseOperation subsections[2];
            Test = parseTest subsections[3];
        }
    )

let parseInput input = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.split "\n\n" |> Array.map parseSection

let run initialStates (monkeys: Monkey array) reliefDivider numRounds =
    let doWork worryDivider monkey state =
        let workOnItem item = 
            let newLevel = monkey.Operation item
            newLevel / worryDivider
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

                            { Items = newItems; NumInspected = state.NumInspected + (if i = monkeyIndex then (bigint itemsAndTargets.Length) else 0UL); }
                        )
        result

    let doWorkX monkey state = doWork reliefDivider monkey state

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
    let reliefDivider = 3I
    let final = run states monkeys reliefDivider numRounds

    //let topInspections =
    //    final |> Array.map (fun x -> x.NumInspected)
    //    |> Array.sortByDescending (fun f -> f) |> Array.take 2

    //let result = topInspections |> Array.reduce (fun a b -> a * b)
    //result
    topInspectionsProduct 2 final
    
let part2 input =
    let parsed = parseInput input
    let states = parsed |> Array.map (fun (state, _) -> state)
    let monkeys = parsed |> Array.map (fun (_, monkey) -> monkey)

    let numRounds = 10000
    let reliefDivider = 1I
    let final = run states monkeys reliefDivider numRounds

    let result = topInspectionsProduct 2 final
    result
