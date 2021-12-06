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

let initBoard = { Spaces = [] }
let initSpace value (row,col) =
    {
        Value = value
        Row = row
        Col = col
        Marked = false
    }
let toBoard spaces = { Spaces = spaces }

let toInt (num:string) =
    match Int32.TryParse num with
    | (true, n) -> n
    | _ -> failwith "No number"
 
let getMoves (line:string) =
    line.Split(',')
    |> Array.map toInt

let buildSpaceLine (line:string) boardIndex =
    line.Split(' ')
    |> Array.filter (fun x -> not (String.IsNullOrWhiteSpace(x)))
    |> Array.map toInt
    |> Array.mapi (fun i x -> initSpace x (boardIndex,i))
    |> Array.toList

let rec boardBuilder boardIndex board boardList (lines:string list) =
    match lines with
    | [] -> 
        match board with
        | Some board -> board :: boardList
        | None -> boardList
    | line::remaining -> 
        if String.IsNullOrWhiteSpace line then
            let newList =
                match board with
                | Some board -> board::boardList
                | None -> boardList
            boardBuilder 0 None newList remaining
        else
            let workingBoard =
                match board with
                | Some board -> board
                | None -> initBoard
            let boardLine = buildSpaceLine line boardIndex
            let newBoard = List.concat (seq { workingBoard.Spaces; boardLine }) |> toBoard
            boardBuilder (boardIndex + 1) (Some newBoard) boardList remaining

        
        

let parser (input:string) =
    let lines = input.Split('\n')
    let moves = getMoves(lines.[0])
    ()

    
