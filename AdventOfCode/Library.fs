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


module ChartCourse =
    type Position = {
        Horizontal : int
        Depth : int
        Aim : int
    }

    type Command =
    | Forward
    | Up
    | Down

    let initialPosition = {
        Horizontal = 0
        Depth = 0
        Aim = 0
    }

    let decodeCommand cmd =
        match cmd with
        | "forward" -> Forward
        | "up" -> Up
        | "down" -> Down
        | _ -> failwith "Unknown command"

    let decodeDistance (dist:string) =
        match System.Int32.TryParse dist with
        | (true, num) -> num
        | _ -> failwith "Unknown distance"

    let mapIt (str:string) =
        match str.Split(" ") with
        | [|cmd; dist|] -> (decodeCommand cmd, decodeDistance dist)
        | _ -> failwith "Unknown initial command"

    let position commands =
        let compute pos (cmd,dist)  =
            match cmd with
            | Forward -> { pos with Horizontal = pos.Horizontal + dist }
            | Up -> { pos with Depth = pos.Depth - dist }
            | Down -> { pos with Depth = pos.Depth + dist }
        commands
        |> List.map mapIt
        |> List.fold compute initialPosition

    let positionWithAim commands =
        let compute pos (cmd,dist) = 
            match cmd with
            | Forward -> 
                { pos with 
                    Horizontal = pos.Horizontal + dist 
                    Depth = pos.Depth + pos.Aim * dist
                }
            | Up -> { pos with Aim = pos.Aim - dist }
            | Down -> { pos with Aim = pos.Aim + dist }

        commands
        |> List.map mapIt
        |> List.fold compute initialPosition

