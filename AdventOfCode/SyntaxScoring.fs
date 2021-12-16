module SyntaxScoring

type SyntaxInput = {
    Lines : string list
}

type SyntaxState = {
    Stack : char list
    SyntaxError : char option
}
let initSyntaxState = {
    Stack = []
    SyntaxError = None
}

let readInput (input:string) =
    let lines =
        input.Split("\r\n")
        |> Array.toList
    { Lines = lines }
    
let parseChar (c : char) state =
    let push = { state with Stack = c::state.Stack }
    let pop remainder = { state with Stack = remainder }
    match (c, state.Stack) with
    | '(',_ -> push
    | ')','('::remainder -> pop remainder
    | '[',_ -> push
    | ']','['::remainder -> pop remainder
    | '{',_ -> push
    | '}','{'::remainder -> pop remainder
    | '<',_ -> push
    | '>','<'::remainder -> pop remainder
    | _ -> { state with SyntaxError = Some c }

let parseLine (line:string) =
    let folder state c =
        match state.SyntaxError with
        | Some _ -> state
        | None -> parseChar c state
    line
    |> Seq.fold folder initSyntaxState

let findCorruptedLines input =
    input.Lines
    |> List.map parseLine
    |> List.choose (fun x -> x.SyntaxError)
    |> List.sort

let scoreCorruptedLines input =
    let scoreChar c =
        match c with 
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1_197
        | '>' -> 25_137
        | _ -> 0
    findCorruptedLines input
    |> List.map scoreChar
    |> List.sum

let completeIncompleteLine state =
    let matchIt c =
        match c with
        | '[' -> ']'
        | '{' -> '}'
        | '<' -> '>'
        | '(' -> ')'
        | _ -> failwith "oh no"
    state.Stack
    |> List.map matchIt

let scoreCompletedLine input =
    let score c =
        match c with 
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' ->  4L
        | _ -> failwith "it broke"
    let calculate total c =
        total * 5L + (score c)

    input
    |> Seq.fold calculate 0L


let findIncompletedLines input =
    input.Lines
    |> List.map parseLine
    |> List.filter (fun x -> x.SyntaxError.IsNone)
    |> List.map completeIncompleteLine
    |> List.map scoreCompletedLine

let chooseMiddleAutocompleteScore scores =
    scores
    |> List.sort
    |> List.skip (scores.Length / 2)
    |> List.head
