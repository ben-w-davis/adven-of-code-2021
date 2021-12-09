module CrabSubmarines
open System
open System.Collections.Generic

let fuelUse (crab:int) (position:int) =
    let result = crab - position
    if result < 0 then -1 * result else result

let mutable fuelTable = new Dictionary<int,int>()

let betterFuelUse (crab:int) (position:int) =
    let rawFuel = fuelUse crab position
    match fuelTable.TryGetValue rawFuel with
    | (true, value) -> value
    | _ -> 
        let fuelUsed = [1 .. rawFuel] |> List.sum
        fuelTable.Add(rawFuel, fuelUsed)
        fuelUsed

let allFuelUse crabs position =
    crabs
    |> List.map (fuelUse position)
    |> List.sum

let bestFuelUse crabs position =
    crabs
    |> List.map (betterFuelUse position)
    |> List.sum

let getAveragePosition crabs = 
    let sum  = List.sum crabs
    sum / crabs.Length

let getHighestPosition crabs = 
    List.max crabs

let getLowestPosition crabs = 
    List.min crabs

let getMostCommonPosition (crabs:int list) = 
    List.groupBy id crabs
    |> List.map (fun (x,xs) -> (x,List.length xs))
    |> List.sortByDescending (fun (x,xs) -> xs)
    |> List.head
    |> fst

let getLowestFuelUsage crabs =
    let lowest = getLowestPosition crabs
    let highest = getHighestPosition crabs

    [lowest .. highest]
    |> List.map (fun x -> (allFuelUse crabs x, x))
    |> List.minBy fst
    |> fst
        
let getBestFuelUsage crabs =
    let lowest = getLowestPosition crabs
    let highest = getHighestPosition crabs
    [lowest .. highest]
    |> List.map (fun x -> (bestFuelUse crabs x, x))
    |> List.minBy fst
    |> fst
