module HydrothermalVents
open System

type Line = {
    x1 : int
    y1 : int
    x2 : int
    y2 : int
}

type Coordinates = {
    x : int
    y : int
}

let toInt (num:string) =
    match Int32.TryParse num with
    | (true, n) -> n
    | _ -> failwith "No number"

let buildLine (x1,y1) (x2,y2) =
    { x1 = x1; y1 = y1; x2 = x2; y2 = y2 }

let parseLine (line:string) =
    let parts = line.Split(' ')
    let firstPair = parts.[0].Split(',')
    let secondPair = parts.[2].Split(',')
    { 
        x1 = firstPair.[0] |> toInt
        y1 = firstPair.[1] |> toInt
        x2 = secondPair.[0] |> toInt
        y2 = secondPair.[1] |> toInt
    }

let parseLines (textInput:string) =
    textInput.Split('\n')
    |> Array.map parseLine

let isSimpleLine line =
    line.x1 = line.x2 || line.y1 = line.y2

let keepHorizontalAndVertical lines =
    lines
    |> Seq.filter isSimpleLine
    |> Seq.toList

let createCoordinatesFromLine line =
    let xdiff = line.x1 - line.x2
    let ydiff = line.y1 - line.y2
    match (xdiff, ydiff) with
    | (0,y) when y > 0  -> 
        [|line.y2 .. line.y1|]
        |> Array.rev
        |> Array.map (fun y -> { x = line.x1; y = y })
        |> Array.toList
    | (0,y) when y < 0 ->
        [|line.y1 .. line.y2|]
        |> Array.map (fun y -> { x = line.x1; y = y })
        |> Array.toList
    | (x,0) when x > 0 ->
        [|line.x2 .. line.x1|]
        |> Array.rev
        |> Array.map (fun x -> { x = x; y = line.y1 })
        |> Array.toList
    | (x,0) when x < 0 ->
        [|line.x1 .. line.x2|]
        |> Array.map (fun x -> { x = x; y = line.y1 })
        |> Array.toList
    | _ ->
        [
        ]
