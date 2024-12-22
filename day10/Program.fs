module day08

open System.IO  

let input = File.ReadAllLines("input")
let example = File.ReadAllLines("example")
let example1 = File.ReadAllLines("example_1")

type Point = int*int
type Height = int
type Position = Point * Height
type TopMap = Position list list
type Trailhead = Point
type HikingTrail = Point list

let readMap (input: string list list) =
    input
    |> List.mapi (fun rowIndex row -> 
        row 
        |> List.mapi (fun colIndex point -> ((colIndex, rowIndex), int point))
    )
    |> List.concat

let findHikingTrailsFromTrailhead (map: TopMap) (trailhead: Trailhead): HikingTrail list
    = []

let score (trails: HikingTrail list) = trails.Length

let getScore (map: TopMap) (trailheads: Trailhead list) : int = 
    trailheads 
    |> List.fold (fun acc trailhead -> acc + score (findHikingTrailsFromTrailhead map trailhead)) 0

let findTrailheads (map: TopMap) =
    map
    |> List.mapi (fun rowIndex row -> 
        row 
        |> List.mapi (fun colIndex point -> (point, (colIndex, rowIndex)))
    )
    |> List.concat
    |> List.filter (fun (point, _) -> point = 0)
    |> List.groupBy fst
    |> List.map (fun (key, values) -> key, List.map snd values)

let solution1 (input: string) : int = 
    let map = readMap input
    getScore map (findTrailheads map)

printfn "%d %d" (findTrailheads example) 1
printfn "%d / %d" (solution1 example) 
printfn "%d / %d" (solution1 example1) 36

    
