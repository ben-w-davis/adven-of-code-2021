module SmokeBasin
open System

let readInput (input:string) =
    let splitLine (str:string) =
        str
        |> Seq.map (Char.GetNumericValue >> int)
        |> Seq.toArray
    input.Split("\r\n")
    |> Array.map splitLine

let getValue (heightmap:int[][]) (x,y) =
    heightmap[y][x]

let getNeighbors (heightmap : int[][]) (x,y) =
    let top = 
        if y > 0 then
            getValue heightmap (x,y-1) |> Some
        else
            None
    let bottom = 
        if y < heightmap.Length - 1 then
            getValue heightmap (x,y+1) |> Some
        else
            None

    let left = 
        if x > 0 then
            getValue heightmap (x-1,y) |> Some
        else
            None
    let right = 
        if x < heightmap[0].Length - 1 then
            getValue heightmap (x+1,y) |> Some
        else
            None

    [left; right; top; bottom] |> List.choose id


