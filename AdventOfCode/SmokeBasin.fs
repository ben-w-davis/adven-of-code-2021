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

let getRiskLevelSum heightmap =
    getLowPoints heightmap
    |> List.map (fun x -> x + 1)
    |> List.sum

type Location = {
    Top : Location option
    Bottom : Location option
    Left : Location option
    Right : Location option
    Position : int * int
    Height : int
}
let emptyLoc position value = 
    {
        Height = value
        Position = position
        Top = None
        Bottom = None
        Left = None
        Right = None
    }

let rec buildLocation (heightmap : int[][]) usedPositions (x,y) =
    let buildLoc (x2,y2) =
        let value = getValue heightmap (x2,y2)
        if value = 9 || usedPositions |> List.contains (x2,y2) then
            None
        else
            buildLocation heightmap ((x2,y2)::usedPositions) (x2,y2)
    let top = 
        if y > 0 then
            buildLoc (x,y-1)
        else
            None
    let bottom = 
        if y < heightmap.Length - 1 then
            buildLoc (x,y+1)
        else
            None

    let left = 
        if x > 0 then
            buildLoc (x-1,y)
        else
            None
    let right = 
        if x < heightmap[0].Length - 1 then
            buildLoc (x+1,y)
        else
            None
    
    {
        Height = getValue heightmap (x,y)
        Position = (x,y)
        Top = top
        Bottom = bottom
        Left = left
        Right = right
    } |> Some

let rec usedPositions location =
    let toList loc = loc |> Option.map usedPositions |> Option.defaultValue []
    [
        [location.Position]
        location.Top |> toList
        location.Bottom |> toList
        location.Left |> toList
        location.Right |> toList
    ] |> List.collect id


type Basin = {
    Positions : (int * int) list
    Size : int
}

let createBasin location =
    let positions = usedPositions location
    {
        Positions = positions
        Size = positions.Length
    }

