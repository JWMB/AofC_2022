module D16

open Tools
open System.Text.RegularExpressions

type Room = { Id: string; Rate: int; Connections: string array}
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
let parseRow row =
    let m = Regex.Match(row, @"Valve (?<valve>\w+).+rate=(?<rate>\d+).+valves? (?<connections>.+)")
    { Id = m.Groups["valve"].Value; Rate = int m.Groups["rate"].Value; Connections = m.Groups["connections"].Value |> RxCurry.splitTrimNoEmpty "," }

type State = { Valves: Map<string, int>; TotalTime: int; TotalSteam: int; TotalFlow: int; History: string array } with
    member this.Update steps =
        { this with
            TotalTime = this.TotalTime + steps;
            TotalSteam = this.TotalSteam + this.TotalFlow * steps;
            //History = this.History |> Array.append [| $"Update {steps} - {this.debugState}" |]
        }

    member this.AddRoomToHistory room =
        this
        //{ this with
        //    History = this.History |> Array.append [| $"Room {room.Id} - {this.debugState}" |]
        //}

    member this.debugState = $"{this.TotalTime} {this.TotalSteam}"
    member this.turnValveIfNotOn room =
        if room.Rate > 0 && this.Valves[room.Id] = 0 then
            let valves = this.Valves |> Map.change room.Id (fun _ -> Some(room.Rate))
            Some({ (this.Update 1) with
                    Valves = valves;
                    TotalFlow = valves.Values |> Seq.sum;
                    //History = this.History |> Array.append [| $"TurnOn {room.Id}/{room.Rate} - {this.debugState}" |]
                })
        else None
    static member create valveStates = { Valves = valveStates; TotalTime = 0; TotalSteam = 0; TotalFlow = 0; History = [||] }

let part1 input =
    let rooms = Parsing.parseRows input parseRow |> Array.map (fun r -> (r.Id, r)) |> Map.ofArray

    let startAt = "AA"
    //let timeToMove = 1
    //let timeToOperate = 1
    let maxTime = 30

    let state = { State.create (rooms |> Map.map (fun _ _ -> 0)) with TotalTime = 0; }

    let maxPossibleFlow = rooms.Values |> Seq.map (fun r -> r.Rate) |> Seq.sum

    let rec traverse (state: State) room roomIdsSinceLastChoice =
        let state = state.AddRoomToHistory room
        if state.TotalTime = maxTime then state
        elif state.TotalTime > maxTime then state
        else
            let otherRooms (state: State) roomIdsSinceLastChoice =
                let connections = room.Connections |> Array.except roomIdsSinceLastChoice
                if connections.Length = 0 then state // nowhere to go, finished
                else
                    let since = 
                        if connections.Length = 1 then roomIdsSinceLastChoice |> Array.append [|room.Id|]
                        else [| room.Id |]
                    let updated = state.Update 1
                    let childStates = connections |> Array.map (fun nextRoomId -> traverse updated rooms[nextRoomId] since)
                    let best = childStates |> Array.maxBy (fun f -> f.TotalSteam)
                    best

            let turnedOn = state.turnValveIfNotOn room

            let isFinal = turnedOn.IsSome && turnedOn.Value.TotalFlow = maxPossibleFlow
            if isFinal then
                turnedOn.Value.Update (maxTime - turnedOn.Value.TotalTime)
            else
                let childStates =
                    [|
                        otherRooms state roomIdsSinceLastChoice
                    |] |> Array.append (
                        // Also if we can turn this on, try that and then continue exploring
                        if turnedOn.IsSome then
                            let newState = turnedOn.Value
                            [| 
                                if newState.TotalTime < maxTime then otherRooms newState [||]
                                else newState
                            |]
                        else [||]
                    )
                let best = childStates |> Array.maxBy (fun f -> f.TotalSteam)
                best

    let best = traverse state rooms[startAt] [||]
    //let history = best.History |> Array.rev
    let result = best.TotalSteam
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
