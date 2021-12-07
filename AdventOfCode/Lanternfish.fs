module Lanternfish

type Lanternfish = {
    Timer : int
}

let createLanternfish internalTimer =
    {
        Timer = internalTimer
    }

let parseFish (inputText:string) =
    inputText.Split(',')
    |> Array.map (int >> createLanternfish)
    |> Array.toList

let timerTick lanternfish =
    match lanternfish.Timer with
    | 0 -> [ createLanternfish 6; createLanternfish 8 ]
    |_ ->
        [
            { lanternfish with Timer = lanternfish.Timer - 1 }
        ]

let timerTickAll fishes =
    fishes
    |> List.collect timerTick

let rec tickDown days fishes =
    if days = 0 then fishes
    else
        timerTickAll fishes
        |> tickDown (days-1)

let runSimulation input days =
    parseFish input
    |> tickDown days

let parseRaw (inputText:string) =
    inputText.Split(',')
    |> Array.map int

let tick (x,xl) =
    match x with
    | 0 -> 
        Array.concat [Array.replicate xl 6; (Array.replicate xl 8)]
    | _ ->
        Array.replicate xl (x-1)

let rec tickRec days fishes =
    if days = 0 then fishes
    else
        fishes
        |> Array.groupBy id
        |> Array.map (fun (x,xs) -> (x,xs.Length))
        |> Array.collect tick
        |> tickRec (days-1)

let tickFast days fishes =
    fishes
    |> tickRec days

let runSimulationFaster input days =
    parseRaw input
    |> tickFast days
