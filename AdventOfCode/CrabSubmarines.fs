module CrabSubmarines
open System

let fuelUse (crab:int) (position:int) =
    let result = crab - position
    if result < 0 then -1 * result else result

let allFuelUse crabs position =
    crabs
    |> List.map (fuelUse position)
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


