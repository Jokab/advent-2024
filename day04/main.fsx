open System.IO  
open System.Text.RegularExpressions;

type Grid = char list list
type Point = int*int

let lines = File.ReadAllLines("input") |> List.ofArray
let lines2 = File.ReadAllLines("input2") |> List.ofArray

let example = File.ReadAllLines("example") |> List.ofArray
let example2 = File.ReadAllLines("example2") |> List.ofArray

let readInput (lst: string list) = 
    lst |> List.map Seq.toList

let collecti clt list =
    list
    |> List.indexed                // Create (index, value) pairs
    |> List.collect (fun (i, x) -> clt i x) // Apply the collector function


let findX (lst: Grid) =
    lst
    |>  collecti (fun outer a -> 
            List.fold (fun (inner, acc) e -> 
                if e = 'X' then (inner+1, (outer,inner) :: acc) else (inner+1, acc)
            ) (0, []) a |> snd
        )

let word = "XMAS";

let nextLetter (currentLetter: char) = 
    match word.IndexOf(currentLetter) with 
    | i when i = word.Length -> None
    | i -> Some word[i + 1]

let getLetter (grid: Grid) (pos: Point) = grid[fst pos][snd pos]

let rec searchDirection (list: Grid) point nextPosition letterToFind =
    match nextPosition list point with
    | Some pos -> match getLetter list pos with
                    | c when c = letterToFind -> 
                        match nextLetter letterToFind with
                            | None -> true
                            | Some letter -> searchDirection list pos nextPosition letter
                    | _ -> false
    | None -> false

let pointExists (list: Grid) (point: Point) =
    match fst point < list.Length && snd point < list[fst point].Length with
    | true -> Some point
    | false -> None

// let nextPosition grid = 
//     match pointExists grid with
//     | 

let searchAllDirections (list: Grid) (xPosition: Point) =
    searchDirection list xPosition (fun grid (a,b) -> pointExists grid (a, b+1)) 'X'

let solution1 input : int = 
    let lst = readInput input
    lst
    |> findX 
    |> List.fold (fun acc i -> acc + (if searchAllDirections lst i then 1 else 0)) 0

printfn "%d ?= %d" (solution1 example) 4
// printfn "%d = %d" (solution1 example2) 18

// printfn "%d" solution2


    
