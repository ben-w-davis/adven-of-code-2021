module Day09
open System
open Xunit
open FsUnit.Xunit
open SmokeBasin

let sampleInput = """2199943210
3987894921
9856789892
8767896789
9899965678"""

[<Fact>]
let ``Read input into heightmap``() =
    let result = readInput sampleInput
    let expected = [|2;1;9;9;9;4;3;2;1;0|]

    result[0] |> should equal expected

[<Fact>]
let ``We can index into middle of heightmap``() =
    let result = readInput sampleInput

    result[1][1] |> should equal 9

[<Fact>]
let ``Get neighbors for item in middle of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (1,1)

    let expected = [3;8;1;8]
    result |> should equal expected

[<Fact>]
let ``Get value from coordinates``() =
    let heightmap = readInput sampleInput
    let result = getValue heightmap (5,4)

    result |> should equal 6

[<Fact>]
let ``Get neighbors for item at top-left of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (0,0)

    let expected = [1;3]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at top-right of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (9,0)

    let expected = [1;1]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at bottom-right of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (9,4)

    let expected = [7;9]
    result |> should equal expected

[<Fact>]
let ``Get neighbors for item at bottom-left of heightmap``() =
    let heightmap = readInput sampleInput
    let result = getNeighbors heightmap (0,4)

    let expected = [8;8]
    result |> should equal expected

[<Fact>]
let ``Use neighbors to check for low point``() =
    let neighbors = [1;2]
    let value = 0
    let result = keepLowPoint neighbors value

    result |> should equal (Some value)

[<Fact>]
let ``Use neighbors to check for low point not being found``() =
    let neighbors = [2;4;4]
    let value = 3
    let result = keepLowPoint neighbors value

    result |> should equal None

[<Fact>]
let ``Search all lines for low points``() =
    let heightmap = readInput sampleInput
    let result = getLowPoints heightmap

    result |> should equal [1;0;5;5]

[<Fact>]
let ``Get sum of risk level``() =
    let heightmap = readInput sampleInput
    let result = getRiskLevelSum heightmap

    result |> should equal 15




    

