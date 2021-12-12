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
let ``Read input into array``() =
    let result = readInput sampleInput
    let expected = [|2;1;9;9;9;4;3;2;1;0|]

    result[0] |> should equal expected
