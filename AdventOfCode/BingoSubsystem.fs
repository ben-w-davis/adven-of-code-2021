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

type GameState = {
    Moves : int list
    Boards : Board list
}

type GameResult = {
    WinningNumber : int
    UnMarked : int
    Score : int
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
let toGameState moves boards = { Moves = moves |> Seq.toList; Boards = boards }
let toResult num total =
    {
        WinningNumber = num
        UnMarked = total
        Score = num * total
    }

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
    let boards = boardBuilder 0 None [] (lines |> Array.skip 2 |> Array.toList)
    toGameState moves boards

let mark numbers space =
    if numbers |> List.contains space.Value then
        { space with Marked = true }
    else
        space

let markBoard board numbers =
    board.Spaces
    |> List.map (mark numbers)
    |> toBoard

let markOne number board = markBoard board [number]

let checkByRow marked =
    marked 
    |> List.groupBy (fun x -> x.Row)
    |> List.map (fun (_,spaces) -> spaces.Length)
    |> List.exists (fun x -> x >= 5)

let checkByColumn marked =
    marked 
    |> List.groupBy (fun x -> x.Col)
    |> List.map (fun (_,spaces) -> spaces.Length)
    |> List.exists (fun x -> x >= 5)

let doesBoardWin board =
    let marked = board.Spaces |> List.filter (fun x -> x.Marked)
    if marked.Length < 5 then false
    else
        (checkByRow marked) || (checkByColumn marked)

let rec runRec gameState =
    match gameState.Moves with
    | [] -> toResult -1 0
    | next::remaining ->
        let boards = gameState.Boards |> List.map (markOne next)
        if boards |> List.exists doesBoardWin then
            let winner = boards |> List.filter doesBoardWin |> List.head
            let unmarked = winner.Spaces |> List.filter (fun x -> x.Marked |> not) |> List.sumBy (fun x -> x.Value)
            toResult next unmarked
        else
            toGameState remaining boards
            |> runRec

let run input =
    parser input
    |> runRec 

