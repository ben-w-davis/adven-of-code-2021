module Day05
open System
open Xunit
open HydrothermalVents

let sampleInput =
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

[<Fact>]
let ``Decode input first line``() =
    let line = "0,9 -> 5,9"
    let result = parseLine line

    Assert.Equal(0, result.x1)
    Assert.Equal(9, result.y1)
    Assert.Equal(5, result.x2)
    Assert.Equal(9, result.y2)
[<Fact>]
let ``Keep only horizontal and vertical lines``() =
    ()
     
