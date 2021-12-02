module Day01

open System
open Xunit
open AdventOfCode

[<Fact>]
let ``Sonar sweep counts increases``() =
    let depthList = 
        [
            199
            200
            208
            210
            200
            207
            240
            269
            260
            263
        ]
    let result = DepthMeasurement.count depthList

    Assert.Equal(7, result)

