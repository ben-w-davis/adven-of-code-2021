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

let keepLowPoint neighbors value =
    let min = neighbors |> List.min
    if value < min then 
        Some value
    else
        None

let getLowPoints (heightmap : int[][]) =
    let line = heightmap |> Array.head
    let makeNeighbors v x y =
        let neighbors = getNeighbors heightmap (x,y)
        (v, neighbors)
    let processLine index line =
        line
        |> Array.mapi (fun i v -> makeNeighbors v i index)
        |> Array.choose (fun (v,n) -> keepLowPoint n v )
        |> Array.toList
    heightmap
    |> Array.mapi (fun i line -> processLine i line)
    |> Seq.collect (fun line -> line)
    |> Seq.toList

