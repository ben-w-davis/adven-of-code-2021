module SmokeBasin
open System

let readInput (input:string) =
    let splitLine (str:string) =
        str
        |> Seq.map (Char.GetNumericValue >> int)
        |> Seq.toArray
    input.Split("\r\n")
    |> Array.map splitLine


