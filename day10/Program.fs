module day08

open System.IO  

let input = File.ReadAllLines("input")
                |> List.ofArray 
                |> List.map Seq.toList
let example = File.ReadAllLines("example")
                |> List.ofArray 
                |> List.map Seq.toList
let example1 = File.ReadAllLines("example1")
                |> List.ofArray 
                |> List.map Seq.toList


type Point = int*int
type Height = int
type Position = Point * Height
type TopMap = Position list list
type Trailhead = Point
type HikingTrail = Point list

let readMap (input: char list list) : TopMap =
    input
    |> List.mapi (fun rowIndex row -> 
        row 
        |> List.mapi (fun colIndex height -> ((colIndex, rowIndex), int height - int '0'))
    )
    
type Direction = UP | RIGHT | DOWN | LEFT
// let directionToDelta direction =
//     match direction
    
let getPositionInDirection (map: TopMap) (((x,y),hi): Position) (direction: Direction) =
    let positionIfExists x y =
        if x >= 0 && x < map[0].Length && y >= 0 && y < map.Length
        then Some (map[y][x])
        else None
    let result = match direction with
                    | UP -> positionIfExists x (y-1)
                    | RIGHT -> positionIfExists (x+1) y
                    | DOWN -> positionIfExists x (y+1)
                    | LEFT -> positionIfExists (x-1) y
    let (rx,ry,h) = match result with
                    | None -> ("-", "-", "-")
                    | Some ((x,y),h) -> (string x, string y, string h)
    result

let findHikingTrailsFromTrailhead (map: TopMap) ((point): Trailhead): int =
    printfn "startposition %A" point
    let mutable visitedNines = set []
    let rec helper (pos: Position option) previousHeight visited =
        match pos with
        | None -> 0
        | Some x ->
            if visited |> Set.contains (fst x)
            then 0
            else
                let newVisited = Set.add (fst x) visited
                match x with
                | (_,_),height when height <> (previousHeight + 1) -> 0  
                | (x,y),height when height = 9 -> 1// && not (visitedNines.Contains((x,y))) ->
                    // visitedNines <- Set.add (x,y) visitedNines
                    // 1
                | (x,y),height ->
                                       printfn $"walking from %d{x},%d{y},%d{height}"
                                       helper (getPositionInDirection map ((x,y),height) Direction.UP) height newVisited
                                       + helper (getPositionInDirection map ((x,y),height) Direction.RIGHT) height newVisited 
                                       + helper (getPositionInDirection map ((x,y),height) Direction.DOWN) height newVisited
                                       + helper (getPositionInDirection map ((x,y),height) Direction.LEFT) height newVisited
    
    let result = helper (Some (point, 0)) -1 (set [])
    printfn "result %d" result
    result

let score (trails: HikingTrail list) = trails.Length

let getScore (map: TopMap) (trailheads: Trailhead list) : int = 
    trailheads 
    |> List.fold (fun acc trailhead -> acc + findHikingTrailsFromTrailhead map trailhead) 0

let findTrailheads (map: TopMap) : Trailhead list = 
    map
    |> List.collect id
    |> List.filter (fun (position, height) -> height = 0)
    |> List.map fst

let solution1 (input: char list list) : int = 
    let map = readMap input
    getScore map (findTrailheads map)

// printfn "%d %d" (findTrailheads example) 1
// printfn $"%d{solution1 example} / 1" 
// printfn $"%d{solution1 example1} / 36" 
printfn $"%d{solution1 input}" 

    
    