module D16

open Tools
open System.Text.RegularExpressions

type Room = { Id: string; Rate: int; Connections: string array}
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
let parseRow row =
    let m = Regex.Match(row, @"Valve (?<valve>\w+).+rate=(?<rate>\d+).+valves? (?<connections>.+)")
    { Id = m.Groups["valve"].Value; Rate = int m.Groups["rate"].Value; Connections = m.Groups["connections"].Value |> RxCurry.splitTrimNoEmpty "," }

type State = { Valves: Map<string, int>; TotalTime: int; TotalSteam: int; TotalFlow: int; History: string array } with
    member this.Update steps historyComment =
        { this with
            TotalTime = this.TotalTime + steps;
            TotalSteam = this.TotalSteam + this.TotalFlow * steps;
            History = this.History |> Array.append [| $"Update {steps} - {this.debugState} {historyComment}" |]
        }

    member this.AddRoomToHistory room =
        //this
        { this with
            History = this.History |> Array.append [| $"Room {room.Id} - {this.debugState}" |]
        }

    member this.turnOnValve room = 
            let valves = this.Valves |> Map.change room.Id (fun _ -> Some(room.Rate))
            { (this.Update 1 "") with
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

let getBest states = states |> Array.maxBy (fun f -> f.TotalSteam)

let run (rooms: Map<string, Room>) maxTime =
    let shortest = findShortestRoutesBetweenValves rooms.Values

    let rec loop (state: State) room =
        let remainingOpen = state.Valves |> Map.filter (fun k v -> v = 0 && rooms[k].Rate > 0) |> Map.toArray |> Array.map (fun (k, _) -> k)

        if remainingOpen.Length = 0 then
            state.Update (maxTime - state.TotalTime) "Done"
        elif state.TotalTime = maxTime then
            state
        else
            if remainingOpen |> Array.contains room.Id then
                let newState = state.turnOnValve room
                loop newState room
            else
                let goTo fromId toId =
                    let route = shortest[(fromId, toId)]
                    let time = min (maxTime - state.TotalTime) route.Length
                    state.Update time $"{fromId}->{toId}"
                let children = remainingOpen |> Array.map (fun dst -> 
                                let best = loop (goTo room.Id dst) rooms[dst]
                                best
                    )
                getBest children
        
    // first we need to get to a room
    let state = { State.create (rooms |> Map.map (fun _ _ -> 0)) with TotalTime = 0; }
    let startAt = "AA"

    let startRoutes = rooms |> Map.filter (fun k v -> v.Rate > 0) |> Map.map (fun k v -> (findShortestRoute rooms rooms[startAt] v).Value)
    let best = startRoutes |> Map.toArray |> Array.map (fun (k, path) -> 
                            let newState = state.Update path.Length $"Start {k}"
                            let finalState = loop newState rooms[k]
                            finalState 
                            ) |> getBest
    best


let part1 input =
    let rooms = Parsing.parseRows input parseRow |> Array.map (fun r -> (r.Id, r)) |> Map.ofArray

    let best = run rooms 30
    let result = best.TotalSteam
    result
    
let part2 input =
    let rooms = Parsing.parseRows input parseRow |> Array.map (fun r -> (r.Id, r)) |> Map.ofArray

    let best = run rooms 26
    let result = best.TotalSteam
    result
