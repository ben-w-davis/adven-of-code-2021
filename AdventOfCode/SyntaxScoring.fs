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
    match (c, state.Stack) with
    | '(',_ -> { state with Stack = c::state.Stack }
    | ')','('::remainder -> { state with Stack = remainder }
    | '[',_ -> { state with Stack = c::state.Stack }
    | ']','['::remainder -> { state with Stack = remainder }
    | '{',_ -> { state with Stack = c::state.Stack }
    | '}','{'::remainder -> { state with Stack = remainder }
    | '<',_ -> { state with Stack = c::state.Stack }
    | '>','<'::remainder -> { state with Stack = remainder }
    | _ -> { state with SyntaxError = Some c }

let parseLine (line:string) =
    let folder state c =
        match state.SyntaxError with
        | Some _ -> state
        | None -> parseChar c state
    line
    |> Seq.fold folder initSyntaxState
