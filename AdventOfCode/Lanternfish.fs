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

let fill  (arr:int64[]) (x,xl) =
    arr.[x] <- xl
    arr

let shift (arr:int64[]) =
    let (new6,new8) = (arr.[0],arr.[0])
    arr.[0] <- arr.[1]
    arr.[1] <- arr.[2]
    arr.[2] <- arr.[3]
    arr.[3] <- arr.[4]
    arr.[4] <- arr.[5]
    arr.[5] <- arr.[6]
    arr.[6] <- arr.[7] + new6
    arr.[7] <- arr.[8]
    arr.[8] <- new8
    arr

let rec tick days (arr:int64[]) =
    if days = 0 then arr
    else
        shift arr
        |> tick (days-1) 

let tickFast days fishes =
    let init = Array.replicate 9 (int64(0))
    let temp =
        fishes
        |> Array.groupBy id
        |> Array.map (fun (x,xs) -> (x,int64(xs.Length)))
    temp 
    |> Array.fold fill init
    |> tick days
    |> Array.sum

let runSimulationFaster input days =
    parseRaw input
    |> tickFast days
