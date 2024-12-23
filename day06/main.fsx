open System.IO  
open System.Text.RegularExpressions;

let lines2 = File.ReadAllLines("input2") |> List.ofArray

let example = File.ReadAllLines("example1_1") |> List.ofArray
let example2 = File.ReadAllLines("example2") |> List.ofArray


let parseMap (lst: string list) = 
    lst |> List.map Seq.toList

type GuardMap = string list list
type Point = int*int

let outsideMap (map: GuardMap) ((x,y): Point) =
    (x < 0 || x >= map.Length) || (y < 0 || y >= map[x].Length)

let visited: Set<Point> = set []

let hasVisited (point: Point) =
    Set.contains point visited

type Direction = UP | RIGHT | DOWN | LEFT

let nextPosition (x,y) curDir = 
    match curDir with
    | UP ->     (x,     y-1)
    | RIGHT ->  (x+1,   y)
    | DOWN ->   (x,     y-1)
    | LEFT ->   (x-1,   y)


let obstacleInFront (map: GuardMap) ((x,y): Point) curDir =
    let (x1,y1) = nextPosition (x,y) curDir
    map[y1][x1] = "#"

let turnRight curDir = 
    match curDir with
    | UP -> RIGHT
    | RIGHT -> DOWN
    | DOWN -> LEFT
    | LEFT -> UP


// let walkForward (x,y) curDir = 
//     match curDir with
//     | UP ->     (x,     y-1)
//     | RIGHT ->  (x+1,   y)
//     | DOWN ->   (x,     y-1)
//     | LEFT ->   (x-1,   y)


let directionSigns = ["^"; "v"; "<"; ">"]

let rec findStartPosition (map: GuardMap) : option<Point * Direction> =
    let rec helper map i = 
        match map with
        | [] -> None
        | head :: tail -> 
            match List.tryFind (fun e -> List.contains e directionSigns) head with
            | Some x -> match x with
                        | "v" -> Some ((i, List.findIndex ((=) x) head), DOWN)
                        | "^" -> Some ((i, List.findIndex ((=) x) head), UP)
                        | "<" -> Some ((i, List.findIndex ((=) x) head), LEFT)
                        | ">" -> Some ((i, List.findIndex ((=) x) head), RIGHT)
                        | _ -> None
            | None -> helper tail (i+1)

    helper map 0

let solution1 input : int = 
    let guardMap = parseMap input
    let startPosition = 
        guardMap 
        |> match findStartPosition with
            | 
    
    let curDir = UP
    match outsideMap
    //     return positions count
    // else 
    //     if not visited
    //         store current position in map
    //     if thing in front
    //         turn right
    //     walk forward
    0
    
let solution2 = 
    ignore

printfn "%d" solution1


    
