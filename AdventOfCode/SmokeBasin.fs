module SmokeBasin
open System

type HeightMap = {
    MapValue : int[][]
    MapWidth : int
    MapHeight : int
}
let createHeightMap (input:int[][]) =
    {
        MapValue = input
        MapWidth = input.[0].Length - 1
        MapHeight = input.Length - 1
    }

let readInput (input:string) =
    let splitLine (str:string) =
        str
        |> Seq.map (Char.GetNumericValue >> int)
        |> Seq.toArray
    input.Split("\r\n")
    |> Array.map splitLine
    |> createHeightMap


let getValue heightmap (x,y) =
    heightmap.MapValue[y][x]

let getNeighbors heightmap (x,y) =
    let top = 
        if y > 0 then
            getValue heightmap (x,y-1) |> Some
        else
            None
    let bottom = 
        if y < heightmap.MapHeight then
            getValue heightmap (x,y+1) |> Some
        else
            None

    let left = 
        if x > 0 then
            getValue heightmap (x-1,y) |> Some
        else
            None
    let right = 
        if x < heightmap.MapWidth then
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

let getLowPoints heightmap =
    let makeNeighbors v x y =
        let neighbors = getNeighbors heightmap (x,y)
        (v, neighbors)
    let processLine index line =
        line
        |> Array.mapi (fun i v -> makeNeighbors v i index)
        |> Array.choose (fun (v,n) -> keepLowPoint n v )
        |> Array.toList
    heightmap.MapValue
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

let rec buildLocation heightmap usedPositions (x,y) =
    let buildLoc (x2,y2) =
        let value = getValue heightmap (x2,y2)
        if value = 9 || usedPositions |> List.contains (x2,y2) then
            None
        else
            let reduced = (x,y)::usedPositions |> List.distinct
            buildLocation heightmap reduced (x2,y2)

    let top () = 
        if y > 0 then
            buildLoc (x,y-1)
        else
            None

    let bottom () = 
        if y < heightmap.MapHeight then
            buildLoc (x,y+1)
        else
            None

    let left () = 
        if x > 0 then
            buildLoc (x-1,y)
        else
            None

    let right () = 
        if x < heightmap.MapWidth then
            buildLoc (x+1,y)
        else
            None

    let value = getValue heightmap (x,y)
    if value = 9 || usedPositions |> List.contains (x,y) then None
    else
        {
            Height = value
            Position = (x,y)
            Top = top ()
            Bottom = bottom ()
            Left = left ()
            Right = right ()
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
    let positions = usedPositions location |> List.distinct
    {
        Positions = positions 
        Size = positions.Length
    }

let findBasins heightmap =
    let allPositions =
        [
            for y in [0 .. heightmap.MapHeight] do
                for x in [0 .. heightmap.MapWidth] do
                    yield x,y
        ]

    let search (state:Basin list) pos =
        match state with
        | [] -> 
            let loc = buildLocation heightmap [] pos
            match loc with
            | Some basinSeed -> 
                let basin = createBasin basinSeed
                printfn "Created Basin #%i, Size: %i" 1 (basin.Size)
                [basin]
            | None -> []
        | basins ->
            let used = basins |> List.collect (fun x -> x.Positions)
            let loc = buildLocation heightmap used pos
            match loc with
            | Some basinSeed -> 
                let basin = createBasin basinSeed
                printfn "Created Basin #%i, Size: %i" 1 (basin.Size)
                basin::basins
            | None -> basins

    allPositions
    |> List.fold search []
    |> List.sortByDescending (fun x -> x.Size)

let basinScore heightmap = 
    findBasins heightmap
    |> List.take 3
    |> List.map (fun x -> x.Size)
    |> List.fold (*) 1

