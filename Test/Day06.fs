module Day06
open System
open Xunit
open Laternfish

let sampleInput = "3,4,3,1,2"

[<Fact>]
let ``Create laternfish from internal timer state``() =
    let internalTimer = 3
    let result = createLaternfish internalTimer

    Assert.Equal(3, result.Timer)
