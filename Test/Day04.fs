﻿module Day04

open System
open Xunit
open BingoSubsystem

let sampleInput = 
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
"""

[<Fact>]
let ``Parse moves list``()=
    let line = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    let result = getMoves line

    Assert.Equal(7, result |> Array.head)
    Assert.Equal(27, result |> Array.length)

[<Fact>]
let ``Build first line of first board``() =
    let line = "22 13 17 11  0"
    let result = buildSpaceLine line 0

    let expected = 
        [
            initSpace 22 (0,0)
            initSpace 13 (0,1)
            initSpace 17 (0,2)
            initSpace 11 (0,3)
            initSpace 0 (0,4)
        ] |> Array.ofList
    Assert.Equal(expected, result)

[<Fact>]
let ``Build board recursively``() =
    let lines = 
        [
            "22 13 17 11  0"
            "8  2 23  4 24"
            "21  9 14 16  7"
            "6 10  3 18  5"
            "1 12 20 15 19"
        ]

    let result = boardBuilder 0 None [] lines

    Assert.Equal(1, result.Length)
    let first = result |> List.head
    Assert.Equal(25, first.Spaces.Length)

[<Fact>]
let ``Parse sample input``() =
    let result = parser sampleInput

    Assert.Equal(27, result.Moves.Length)
    Assert.Equal(3, result.Boards.Length)

