module day04

open System.IO  

type Grid = char list list
type Point = int*int

let lines = File.ReadAllLines("input") |> List.ofArray
let lines2 = File.ReadAllLines("input2") |> List.ofArray

let example = File.ReadAllLines("example") |> List.ofArray
let example2 = File.ReadAllLines("example2") |> List.ofArray

let readInput (lst: string list) = 
    lst |> List.map Seq.toList

let collecti clt =
    List.indexed                
    >> List.collect (fun (i, x) -> clt i x)

let findAllOfLetter (lst: Grid) letter =
    collecti (fun outer a -> 
        List.fold (fun (inner, acc) e -> 
            if e = letter then (inner+1, (outer,inner) :: acc) else (inner+1, acc)
        ) (0, []) a |> snd
    ) lst

let word = "XMAS";

let nextLetter (currentLetter: char) = 
    match word.IndexOf(currentLetter) with 
    | i when i = word.Length - 1 -> None
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

let pointExists (list: Grid) ((a,b): Point) =
    match a >= 0 && a < list.Length && b >= 0 && b < list[a].Length with
    | true -> Some (a,b)
    | false -> None

let directions  =
    [
        // fun (a,b) -> (a,b)
        fun (a,b) -> (a,b+1)
        fun (a,b) -> (a,b-1)
        
        fun (a,b) -> (a+1,b)
        fun (a,b) -> (a+1,b+1)
        fun (a,b) -> (a+1,b-1)
        
        fun (a,b) -> (a-1,b)
        fun (a,b) -> (a-1,b+1)
        fun (a,b) -> (a-1,b-1)
    ]

let searchAllDirections (list: Grid) (xStartPosition: Point) =
    directions |> List.fold (fun acc dir -> acc + if (searchDirection list xStartPosition (fun grid (a,b) -> pointExists grid (dir (a,b))) 'M') then 1 else 0) 0 

let solution1 input : int = 
    let lst = readInput input
    findAllOfLetter lst 'X' 
    |> List.sumBy (searchAllDirections lst)
      
let spellsMasWith deltas grid ((a,b): Point)  =
    deltas
    |> List.map (fun delta -> 
        match pointExists grid (fst delta + a, snd delta + b) with
        | Some x -> getLetter grid x
        | None -> '.')
    |> (fun letters -> letters = ['M'; 'S'] || letters = ['S'; 'M'])

let solution2 input : int =
    let lst = readInput input
    let leftDashSpellsMas = spellsMasWith [(-1, -1); (1,1)]
    let rightDashSpellsMas = spellsMasWith [(-1, 1); (1,-1)]
    findAllOfLetter lst 'A' 
    |> List.sumBy (fun i -> if leftDashSpellsMas lst i && rightDashSpellsMas lst i then 1 else 0)

printfn "%d ?= 2434" <| solution1 lines 
printfn "%d ?= 1835" <| solution2 lines


    
