module BingoSubsystem
open System

type Space = {
    Value : int
    Row : int
    Col : int
    Marked : bool
}

type Board = {
    Spaces : Space list
}

let initSpace value (row,col) =
    {
        Value = value
        Row = row
        Col = col
        Marked = false
    }

let toInt (num:string) =
    match Int32.TryParse num with
    | (true, n) -> n
    | _ -> failwith "No number"
 
let getMoves (textInput:string) =
    let lines = textInput.Split('\n')
    let firstLine = lines |> Array.head
    firstLine.Split(',')
    |> Array.map toInt

let buildSpaceLine (line:string) =
    line.Split(' ')
    |> Array.filter (fun x -> not (String.IsNullOrWhiteSpace(x)))
    |> Array.map toInt
    |> Array.mapi (fun i x -> initSpace x (0,i))
    |> Array.toList

