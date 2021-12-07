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

[<Fact>]
let ``Create fish from input line``() =
    let fishes = parseFish sampleInput
    let expected = [|
        createLanternfish 3
        createLanternfish 4
        createLanternfish 3
        createLanternfish 1
        createLanternfish 2
    |]
    Assert.Equal(expected, fishes)

[<Fact>]
let ``Run sample input for 18 days``() =
    let result = runSimulation sampleInput 18
    Assert.Equal(26, result.Length)

[<Fact>]
let ``Run sample input for 80 days``() =
    let result = runSimulation sampleInput 80
    Assert.Equal(5934, result.Length)
