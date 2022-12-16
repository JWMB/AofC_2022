module D16

open Tools
open System.Text.RegularExpressions

type Room = { Id: string; Rate: int; Connections: string array}
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
let parseRow row =
    let m = Regex.Match(row, @"Valve (?<valve>\w+).+rate=(?<rate>\d+).+valves? (?<connections>.+)")
    { Id = m.Groups["valve"].Value; Rate = int m.Groups["rate"].Value; Connections = m.Groups["connections"].Value |> RxCurry.splitTrimNoEmpty "," }

type State = { Valves: Map<string, int>; TotalTime: int}
let part1 input =
    let rooms = Parsing.parseRows input parseRow |> Array.map (fun r -> (r.Id, r)) |> Map.ofArray

    let startAt = "AA"
    let timeToMove = 1
    let timeToOperate = 1
    let maxTime = 30

    let valveStates = rooms |> Map.map (fun _ _ -> 0)
    let state = { Valves = valveStates; TotalTime = 0 }

    let xx = valveStates.Change("AA", (fun f x -> Some(x)))
    //let rec traverse state currentRoomId =
    //    if state.TotalTime = maxTime then state
    //    else
    //        let room = rooms[currentRoomId]
    //        if room.Rate > 0 && valveStates[currentRoomId] = 0 then
    //            let newState = { Valves = state.Valves.Change(currentRoomId, Some(room.Rate)); TotalTime = state.TotalTime + timeToOperate }
    //            traverse newState currentRoomId

    //        let connections = room.Connections
    //        connections |> Array.map (fun id -> 
    //            let newState = { Valves = state.Valves; TotalTime = state.TotalTime + timeToMove }
    //            traverse newState id
    //        )

    let result = 0
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
