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
            History = this.History |> Array.append [| $"Update {steps} - {this.debugState}" |]
        }

    member this.AddRoomToHistory room =
        //this
        { this with
            History = this.History |> Array.append [| $"Room {room.Id} - {this.debugState}" |]
        }

    member this.turnOnValve room = 
            let valves = this.Valves |> Map.change room.Id (fun _ -> Some(room.Rate))
            { (this.Update 1) with
                    Valves = valves;
                    TotalFlow = valves.Values |> Seq.sum;
                    History = this.History |> Array.append [| $"TurnOn {room.Id} - {this.debugState}" |]
                }

    member this.debugState = $"{this.TotalTime} {this.TotalSteam}"
    member this.turnValveIfNotOn room =
        if room.Rate > 0 && this.Valves[room.Id] = 0 then Some(this.turnOnValve room)
        else None
    static member create valveStates = { Valves = valveStates; TotalTime = 0; TotalSteam = 0; TotalFlow = 0; History = [||] }

let addTo arr item = [| item |] |> Array.append arr

let findShortestRoute (roomsById: Map<string, Room>) fromRoom toRoom =
    let rec traverse room history =
        let choices = room.Connections |> Array.except history
        if choices |> Array.contains toRoom.Id then
            Some(addTo history toRoom.Id)
        else
            if choices.Length = 0 then
                None
            else
                let found = choices |> Array.map (fun c -> traverse roomsById[c] (addTo history c ))
                let found = found |> Array.filter (fun f -> f.IsSome) |> Array.map (fun f -> f.Value)
                if found.Length = 0 then
                    None
                else
                    Some(found |> Array.minBy (fun f -> f.Length))
            
    traverse fromRoom [||]

let findShortestRoutesBetweenValves rooms =
    let rec combine numItems list = 
        match numItems, list with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (combine (k-1) xs) @ combine k xs

    let roomsWithValves = rooms |> Seq.filter (fun f -> f.Rate > 0) |> Seq.toArray
    let roomsById = rooms |> Seq.map (fun r -> (r.Id, r)) |> Map

    let allPairs = combine 2 (roomsWithValves |> Array.toList)
                        |> Seq.toList |> List.map (fun lst -> [ lst; lst |> List.rev ]) // both directions
                        |> List.reduce List.append // flatten

    let withShortest = (allPairs |> List.map (fun lst -> (lst[0].Id, lst[1].Id), (findShortestRoute roomsById lst[0] lst[1]).Value))
    let map = withShortest |> Map.ofList
    map

let part1 input =
    let rooms = Parsing.parseRows input parseRow |> Array.map (fun r -> (r.Id, r)) |> Map.ofArray

    let shortest = findShortestRoutesBetweenValves rooms.Values

    let startAt = "AA"
    //let timeToMove = 1
    //let timeToOperate = 1
    let maxTime = 30

    let state = { State.create (rooms |> Map.map (fun _ _ -> 0)) with TotalTime = 0; }

    let maxPossibleFlow = rooms.Values |> Seq.sumBy (fun f -> f.Rate)

    let rec loop (state: State) room roomIdsSinceLastChoice =
        let remainingOpen = state.Valves |> Map.filter (fun k v -> v = 0 && rooms[k].Rate > 0) |> Map.toArray |> Array.map (fun (k, _) -> k)

        let goTo fromId toId =
            let route = shortest[(fromId, toId)]
            let time = min (maxTime - state.TotalTime) route.Length
            state.Update time

        if remainingOpen.Length = 0 then
            state.Update (maxTime - state.TotalTime)
        elif state.TotalTime = maxTime then
            state
        elif state.TotalTime > maxTime then
            state
        else
            let openIfPossible = 
                if remainingOpen |> Array.contains room.Id then
                    let newState = state.turnOnValve room
                    [| loop newState room [||] |]
                else
                    [| |]

            let proceedWithoutOpening = 
                // Not turning the valve on - no need to go back to previous "linear" locations (where we didn't make any choices)
                let potentialTargets = remainingOpen |> Array.except roomIdsSinceLastChoice |> Array.except [| room.Id |]
                if potentialTargets.Length = 0 then
                    [| state.Update (maxTime - state.TotalTime) |]
                else
                    let roomIdsSinceLastChoice = if potentialTargets.Length = 1 then [| room.Id |] else addTo roomIdsSinceLastChoice room.Id
                    let children = potentialTargets |> Array.map (fun dst -> 
                                                let best = loop (goTo room.Id dst) rooms[dst] roomIdsSinceLastChoice
                                                best
                                                )
                    [| children |> Array.maxBy (fun f -> f.TotalSteam) |]

            let options = proceedWithoutOpening |> Array.append openIfPossible
            //let options = [| |] 
            //            |> Array.append (
            //                if remainingOpen |> Array.contains room.Id then
            //                    let newState = state.turnOnValve room
            //                    [| loop newState room [||] |]
            //                else
            //                    [| |]
            //                )
            //            |> Array.append (
            //                // Not turning the valve on - no need to go back to previous "linear" locations (where we didn't make any choices)
            //                let potentialTargets = remainingOpen |> Array.except roomIdsSinceLastChoice |> Array.except [| room.Id |]
            //                if potentialTargets.Length = 0 then
            //                    [| state.Update (maxTime - state.TotalTime) |]
            //                else
            //                    let roomIdsSinceLastChoice = if potentialTargets.Length = 1 then [| room.Id |] else addTo roomIdsSinceLastChoice room.Id
            //                    let children = potentialTargets |> Array.map (fun dst -> 
            //                                                let best = loop (goTo room.Id dst) rooms[dst] roomIdsSinceLastChoice
            //                                                best
            //                                                )
            //                    [| children |> Array.maxBy (fun f -> f.TotalSteam) |]
            //            )
            let best = options |> Array.maxBy (fun f -> f.TotalSteam)
            best

    // first we need to get to a room
    let startRoutes = rooms |> Map.filter (fun k v -> v.Rate > 0) |> Map.map (fun k v -> (findShortestRoute rooms rooms[startAt] v).Value)
    let osso = startRoutes |> Map.toArray |> Array.map (fun (k, path) -> 
                            let newState = state.Update path.Length
                            let finalState = loop newState rooms[k] [||]
                            finalState 
                            ) |> Array.maxBy (fun f -> f.TotalSteam)
    let aa = osso
    //let aa = ooo state rooms[startAt]


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
