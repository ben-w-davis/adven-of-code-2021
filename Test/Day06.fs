module Day06
open System
open Xunit
open Lanternfish

let sampleInput = "3,4,3,1,2"
let puzzleInput = "1,2,5,1,1,4,1,5,5,5,3,4,1,2,2,5,3,5,1,3,4,1,5,2,5,1,4,1,2,2,1,5,1,1,1,2,4,3,4,2,2,4,5,4,1,2,3,5,3,4,1,1,2,2,1,3,3,2,3,2,1,2,2,3,1,1,2,5,1,2,1,1,3,1,1,5,5,4,1,1,5,1,4,3,5,1,3,3,1,1,5,2,1,2,4,4,5,5,4,4,5,4,3,5,5,1,3,5,2,4,1,1,2,2,2,4,1,2,1,5,1,3,1,1,1,2,1,2,2,1,3,3,5,3,4,2,1,5,2,1,4,1,1,5,1,1,5,4,4,1,4,2,3,5,2,5,5,2,2,4,4,1,1,1,4,4,1,3,5,4,2,5,5,4,4,2,2,3,2,1,3,4,1,5,1,4,5,2,4,5,1,3,4,1,4,3,3,1,1,3,2,1,5,5,3,1,1,2,4,5,3,1,1,1,2,5,2,4,5,1,3,2,4,5,5,1,2,3,4,4,1,4,1,1,3,3,5,1,2,5,1,2,5,4,1,1,3,2,1,1,1,3,5,1,3,2,4,3,5,4,1,1,5,3,4,2,3,1,1,4,2,1,2,2,1,1,4,3,1,1,3,5,2,1,3,2,1,1,1,2,1,1,5,1,1,2,5,1,1,4"

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

[<Fact>]
let ``Run puzzle input for 80 days``() =
    let result = runSimulation puzzleInput 80
    Assert.Equal(356_190, result.Length)

[<Fact>]
let ``Run puzzle input for 80 days - fast``() =
    let result = runSimulationFaster puzzleInput 80
    Assert.Equal(356_190L, result)

[<Fact>]
let ``Run puzzle input for 256 days - fast``() =
    let result = runSimulationFaster puzzleInput 256
    Assert.Equal(1_617_359_101_538L, result)
