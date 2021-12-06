module HydrothermalVents
open System

type Line = {
    x1 : int
    y1 : int
    x2 : int
    y2 : int
}

let toInt (num:string) =
    match Int32.TryParse num with
    | (true, n) -> n
    | _ -> failwith "No number"

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


