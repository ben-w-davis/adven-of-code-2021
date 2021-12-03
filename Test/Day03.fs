module Day03

open System
open Xunit
open PowerConsumption

let sampleInput =
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]

[<Fact>]
let ``Gamma rate of first bit``() =
    let sample = 
        [
            Zero
            One
            One
            One
            One
            Zero
            Zero
            One
            One
            One
            Zero
            Zero
        ]

    let result = gammaBit sample

    Assert.Equal(One, result)


let compareBitList (expected : Bit List) (result : Bit list) =
    expected
    |> List.zip result
    |> List.iter (fun (e,r) -> Assert.Equal(e,r))

[<Fact>]
let ``Convert string bit list to Bits``() =
    let sample = "00100"
    let result = decodeBits sample
    let expected = [Zero;Zero;One;Zero;Zero]

    compareBitList expected result

[<Fact>]
let ``Get gamma for a single report list``() =
    let sample = 
        [
            [Zero;Zero;One;Zero;Zero]
        ]

    let result = gamma sample
    let expected = [Zero;Zero;One;Zero;Zero]

    compareBitList expected result

