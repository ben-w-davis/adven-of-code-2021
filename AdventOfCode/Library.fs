namespace AdventOfCode

module DepthMeasurement =
    let checkForIncrease (x,y) = if y > x then 1 else 0

    let count depths =
        depths
        |> List.pairwise
        |> List.map checkForIncrease
        |> List.sum

    let windowSum (depths: int List) =
        depths |> List.sum

    let windowCount depths =
        depths
        |> List.windowed 3 
        |> List.map windowSum
        |> count

