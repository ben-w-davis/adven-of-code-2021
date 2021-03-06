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
let ``Decode whole sample input``()=
    let lines = parseLines sampleInput

    Assert.Equal(10, lines.Length)
    Assert.Equal(4, lines.[7].y2)

[<Fact>]
let ``Identify keeper lines``() =
    let goodLine = buildLine (0,9) (5,9)
    let badLine = buildLine (8,0) (0,8)
    Assert.True(isSimpleLine goodLine)
    Assert.False(isSimpleLine badLine)

[<Fact>]
let ``Keep only horizontal and vertical lines``() =
    let lines = parseLines sampleInput
    let goodLines = keepHorizontalAndVertical lines
    
    Assert.Equal(6, goodLines.Length)
     

[<Fact>]
let ``Get coordinates from line``() =
    let line = parseLine "2,2 -> 2,1"
    let result = createCoordinatesFromLine line
    let expected =
        [|
            { x = 2; y = 2 }
            { x = 2; y = 1 }
        |]

    Assert.Equal(expected, result)

[<Fact>]
let ``Get coordinates from another line``() =
    let line = parseLine "7,0 -> 7,4"
    let result = createCoordinatesFromLine line
    let expected =
        [|
            { x = 7; y = 0 }
            { x = 7; y = 1 }
            { x = 7; y = 2 }
            { x = 7; y = 3 }
            { x = 7; y = 4 }
        |]
    Assert.Equal(expected, result)

[<Fact>]
let ``Get all horizontal and vertical coordinates from lines``() =
    let lines = parseLines sampleInput |> keepHorizontalAndVertical
    let result = createAllCoordinates lines

    Assert.Equal (5, result |> List.length)

[<Fact>]
let ``Get all horizontal and vertical coordinates from puzzle input``() =
    let input = System.IO.File.ReadAllText "day05_input.txt"
    let lines = parseLines input |> keepHorizontalAndVertical
    let result = createAllCoordinates lines

    Assert.Equal (6_841, result.Length)

[<Fact>]
let ``Create diagonal coordinates``() =
    let line = parseLine "1,1 -> 3,3"
    let result = createDiagonal line

    let expected = 
        [|
            coord (1,1)
            coord (2,2)
            coord (3,3)
        |]
    Assert.Equal(expected, result)

[<Fact>]
let ``Get all coordinates from lines``() =
    let lines = parseLines sampleInput  |> List.ofArray
    let result = createAllCoordinates lines

    Assert.Equal (12, result |> List.length)

[<Fact>]
let ``Get all coordinates from puzzle input``() =
    let input = System.IO.File.ReadAllText "day05_input.txt"
    let lines = parseLines input |> Array.toList
    let result = createAllCoordinates lines

    Assert.Equal (19_258, result.Length)
