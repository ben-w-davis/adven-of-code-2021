module SevenSegmentDisplay

type Digit = {
    Code : string
    Length : int
}

type KnownDigit = {
    StringCode : string
    Value : int
    StringLength : int
}

let toKnownDigit digit value =
    {
        StringCode = digit.Code
        StringLength = digit.Length
        Value = value
    }

type Line = {
    Display : Digit list
    Output : Digit list
}

let createDigit (input:string) =
    let clean = input.Replace("\r", "")
    {
        Code = clean
        Length = clean.Length
    }

let toDigitList (str:string) = str.Split(' ') |> Array.map createDigit |> Array.toList

let createLine (input:string) =
    let parts = input.Split('|')
    {
        Display = parts.[0] |> toDigitList
        Output = parts.[1] |> toDigitList
    }

let getLines (input:string) =
    input.Split('\n')
    |> Array.map createLine
    |> Array.toList

let identifyDigitValue digit =
    match digit.Length with
    | 2 -> Some 1
    | 3 -> Some 7
    | 4 -> Some 4
    | 7 -> Some 8
    | _ -> None

let identifyDigit digit =
    match digit.Length with
    | 2 -> toKnownDigit digit 1 |> Some
    | 3 -> toKnownDigit digit 7 |> Some 
    | 4 -> toKnownDigit digit 4 |> Some 
    | 7 -> toKnownDigit digit 8 |> Some 
    | _ -> None

let identifyAllOutputDigits lines =
    lines
    |> List.collect (fun x -> x.Output)
    |> List.choose identifyDigitValue

let figureTop digits =
    let known = digits |> List.choose identifyDigit
    let seven = known |> List.filter (fun x -> x.Value = 7) |> List.head
    let one = known |> List.filter (fun x -> x.Value = 1) |> List.head
    seven.StringCode |> Seq.except one.StringCode |> Seq.head |> string

