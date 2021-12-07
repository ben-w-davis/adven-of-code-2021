module Lanternfish

type Lanternfish = {
    Timer : int
}

let createLanternfish internalTimer =
    {
        Timer = internalTimer
    }

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
