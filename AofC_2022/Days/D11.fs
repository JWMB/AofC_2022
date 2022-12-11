module D11

open Tools
open System.Text.RegularExpressions

let parseRow row = [| row |]

//type MonkeyTest = { Execute: int -> bool; TrueTarget: int; FalseTarget: int }
type Monkey = { Operation: int -> int; Test: int -> int }
type MonkeyState = { Items: int array; NumInspected: int }

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
            | _ as v -> int v

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
        let divisibleBy = int Regex.Match(str, @"divisible by (\d+)").Groups[1].Value
        let branches = Regex.Matches(str, @"If (true|false).+(\d+)", RegexOptions.Multiline) |> Seq.map (fun m -> (m.Groups[1].Value = "true", int m.Groups[2].Value)) |> Map.ofSeq
        let f v = branches[v % divisibleBy = 0]
        f
    (
        {
            Items = afterColon subsections[1] |> RxCurry.splitTrimNoEmpty "," |> Array.map (fun f -> int f);
            NumInspected = 0;
        },
        {
            Operation = parseOperation subsections[2];
            Test = parseTest subsections[3];
        }
    )

let parseInput input = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.split "\n\n" |> Array.map parseSection

let part1 input =
    let monkeys = parseInput input

    let doWork monkey =
        let workOnItem item = 
            let newLevel = monkey.Operation item
            newLevel / 3
        let itemsAndTargets =
            monkey.Items |> Array.map (fun item ->
                let newLevel = workOnItem item
                let passToMonkeyIndex = monkey.Test newLevel
                (newLevel, passToMonkeyIndex)
            )
        itemsAndTargets

    let execute (monkeys: Monkey array) monkeyIndex = 
        let itemsAndTargets = doWork monkeys[monkeyIndex]
        let result = monkeys |> Array.mapi (fun i m -> 
                            let newItems =
                                if i = monkeyIndex then
                                    [||]
                                else
                                    let itemsToThis = itemsAndTargets |> Array.filter (fun (_, target) -> target = i) |> Array.map (fun (item, _) -> item)
                                    itemsToThis |> Array.append monkeys[i].Items

                            { Items = newItems; Operation = monkeys[i].Operation; Test = monkeys[i].Test }
                        )
        result

    let doRound monkeys =
        let afterRound = 
            [|0..((Array.length monkeys) - 1)|]
            |> Array.fold (fun agg i -> 
                //let numItems = agg[i].Items
                execute agg i) monkeys
        afterRound

    let numRounds = 20
    let final = [|1..numRounds|] |> Array.fold (fun agg _ -> doRound agg) monkeys

    let result = 0
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
