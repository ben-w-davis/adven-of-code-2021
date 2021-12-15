module Day10
open System
open Xunit
open FsUnit.Xunit

open SyntaxScoring

let sampleInput = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

[<Fact>]
let ``Read first line``() =
    let result = readInput sampleInput

    result.Lines |> List.head |> should equal "[({(<(())[]>[[{[]{<()<>>"

[<Fact>]
let ``Parse open paren characters``() =
    let result = parseChar '(' initSyntaxState
    result.Stack |> should equal ['(']

[<Fact>]
let ``Parse open bracket characters``() =
    let result = parseChar '[' initSyntaxState
    result.Stack |> should equal ['[']

[<Fact>]
let ``Parse open curly characters``() =
    let result = parseChar '{' initSyntaxState
    result.Stack |> should equal ['{']

[<Fact>]
let ``Parse open angle characters``() =
    let result = parseChar '<' initSyntaxState
    result.Stack |> should equal ['<']

[<Fact>]
let ``Parse close paren characters``() =
    let result = parseChar ')' { initSyntaxState with Stack = ['('] }
    result.Stack |> should be Empty 

[<Fact>]
let ``Parse close bracket characters``() =
    let result = parseChar ']' { initSyntaxState with Stack = ['['] }
    result.Stack |> should be Empty

[<Fact>]
let ``Parse close curly characters``() =
    let result = parseChar '}' { initSyntaxState with Stack = ['{'] }
    result.Stack |> should be Empty

[<Fact>]
let ``Parse close angle characters``() =
    let result = parseChar '>' { initSyntaxState with Stack = ['<'] }
    result.Stack |> should be Empty

[<Fact>]
let ``Parse close paren characters as error``() =
    let result = parseChar ')' initSyntaxState
    result.SyntaxError |> should equal (Some ')')

[<Fact>]
let ``Parse close bracket characters as error``() =
    let result = parseChar ']' initSyntaxState
    result.SyntaxError |> should equal (Some ']')

[<Fact>]
let ``Parse close curly characters as error``() =
    let result = parseChar '}' initSyntaxState
    result.SyntaxError |> should equal (Some '}')

[<Fact>]
let ``Parse close angle characters as error``() =
    let result = parseChar '>' initSyntaxState
    result.SyntaxError |> should equal (Some '>')

[<Fact>]
let ``Parse first line of sample input``() =
    let line = "[({(<(())[]>[[{[]{<()<>>"
    let result = parseLine line

    result.SyntaxError |> should equal None

[<Theory>]
[<InlineData("{([(<{}[<>[]}>{[]{[(<()>", '}')>]
[<InlineData("[[<[([]))<([[{}[[()]]]", ')')>]
[<InlineData("[{[{({}]{}}([{[{{{}}([]", ']')>]
[<InlineData("[<(<(<(<{}))><([]([]()", ')')>]
[<InlineData("<{([([[(<>()){}]>(<<{{", '>')>]
let ``Parse corrupted line``(line, expected) =
    let result = parseLine line

    result.SyntaxError |> should equal (Some expected)
