module Day07
open System
open Xunit
open CrabSubmarines

let sampleInput = "16,1,2,0,4,2,7,1,2,14"

[<Theory>]
[<InlineData(16,2,14)>]
[<InlineData(1,2,1)>]
[<InlineData(2,2,0)>]
[<InlineData(0,2,2)>]
let ``Calculate fuel usage for one crab``(crab, position, expected) =
    let result = fuelUse crab position

    Assert.Equal(expected, result)

[<Fact>]
let ``Calculate fuel usage for many crabs``() =
    let crabs = [16;1;2;0;4;2;7;1;2;14]
    let position = 2
    let result = allFuelUse crabs position

    Assert.Equal(37, result)
