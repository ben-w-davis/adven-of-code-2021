module Day06
open System
open Xunit
open Lanternfish

let sampleInput = "3,4,3,1,2"

[<Fact>]
let ``Create lanternfish from internal timer state``() =
    let internalTimer = 3
    let result = createLanternfish internalTimer

    Assert.Equal(3, result.Timer)

[<Fact>]
let ``Lanternfish ticks down by day``() =
    let fish = createLanternfish 3
    let result = timerTick fish

    let expected = [|createLanternfish 2|]
    Assert.Equal(expected, result)

[<Fact>]
let ``Lanternfish ticks down by day to create new fish``() =
    let fish = createLanternfish 0
    let result = timerTick fish

    let expected = 
        [|
            createLanternfish 6
            createLanternfish 8
        |]
    Assert.Equal(expected, result)


[<Fact>]
let ``Timer tick down across multiple fish``() =
    let fishes =
        [
            createLanternfish 1
            createLanternfish 3
        ]
    let  result = timerTickAll fishes
    let expected =
        [|
            createLanternfish 0
            createLanternfish 2
        |]
    Assert.Equal(expected, result)
