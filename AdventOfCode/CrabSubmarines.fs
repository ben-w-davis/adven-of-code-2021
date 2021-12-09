module CrabSubmarines
open System

let fuelUse (crab:int) (position:int) =
    let result = crab - position
    if result < 0 then -1 * result else result

let fuelTable =
    [1 .. 2000]
    |> List.map (fun n -> (n, [1..n] |> List. sum))
    |> dict

let betterFuelUse (crab:int) (position:int) =
    let temp = fuelTable
    let rawFuel = fuelUse crab position
    match temp.TryGetValue rawFuel with
    | (true, value) -> value
    | _ -> [1 .. rawFuel] |> List.sum

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
