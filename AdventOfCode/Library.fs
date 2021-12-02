namespace AdventOfCode

module DepthMeasurement =
    let count depths =
        let checkForIncrease (x,y) =
            if y > x then 1 else 0
        depths
        |> List.pairwise
        |> List.map checkForIncrease
        |> List.sum

