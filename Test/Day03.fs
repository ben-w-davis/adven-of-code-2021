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

    compareBitList [Zero;Zero;One;Zero;Zero] result

[<Fact>]
let ``Get gamma for a single report list``() =
    let sample = 
        [
            [Zero;Zero;One;Zero;Zero]
        ]

    let result = gamma sample

    compareBitList [Zero;Zero;One;Zero;Zero] result

[<Fact>]
let ``Get gamma from sample list``() =
    let result = gammaFromRaw sampleInput

    compareBitList [One;Zero;One;One;Zero] result

[<Fact>]
let ``Get epsilon from gamma``() =
    let gamma = [One;Zero;One;One;Zero]
    let result = epsilon gamma

    compareBitList [Zero;One;Zero;Zero;One] result

[<Fact>]
let ``Get decimal value``() =
    let gamma = [One;Zero;One;One;Zero]
    let result = toDecimal gamma

    Assert.Equal(22, result)

[<Fact>]
let ``Get power consumption total``() =
    let result = getTotal sampleInput

    Assert.Equal(198, result)
