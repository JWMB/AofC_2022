module D10

open Tools


type State = { CycleNum: int; X: int }

let parseCommand cmd = match cmd |> Array.head with
                            | "addx" -> { CycleNum = 2 ; X = int cmd[1]; }
                            | "noop" -> { CycleNum = 1 ; X = 0; }
                            | _ -> failwith ""

let parseRow row = RxCurry.split "\s" row |> parseCommand

let part1 input =
    let rows = Parsing.parseRows input parseRow

    let final = rows |> Seq.fold (fun (state, history) change ->
                let newState = { CycleNum = change.CycleNum + state.CycleNum ; X = change.X + state.X; }

                let remainder cycle = (cycle + 40 - 20) % 40

                if remainder state.CycleNum > remainder newState.CycleNum then
                    let stateToLog =
                        let overshoot = remainder newState.CycleNum
                        { CycleNum = newState.CycleNum - overshoot; X = state.X }
                    (newState, [|stateToLog|] |> Array.append history)
                else (newState, history)
                ) ({ CycleNum = 0; X = 1; }, [||])

    let result = (snd final) |> Array.map (fun f -> f.CycleNum * f.X) |> Array.sum

    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow

    let sequenced = rows |> Seq.scan (fun (currState, _) change ->
                        let newState = { CycleNum = change.CycleNum ; X = change.X + currState.X; }
                        (newState, {1..newState.CycleNum} |> Seq.map (fun f -> currState.X))
                            ) ({ CycleNum = 0; X = 1; }, Seq.empty)

    let flattened = sequenced |> Seq.map (fun (_, seq) -> seq) |> Seq.reduce Seq.append |> Seq.toArray

    let renderRow arr = arr |> Array.indexed |> Array.map (fun (index, value) -> 
                                                    let diff = value - index
                                                    if abs diff <= 1 then "#" else "."
                                                    ) |> String.concat ""
    let result = flattened |> Array.chunkBySize 40 |> Array.map renderRow |> String.concat "\n"
    result
