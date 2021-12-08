module CrabSubmarines
open System

let fuelUse (crab:int) (position:int) =
    let result = crab - position
    if result < 0 then -1 * result else result

let allFuelUse crabs position =
    crabs
    |> List.map (fuelUse position)
    |> List.sum

