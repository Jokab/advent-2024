open System.IO  
open System.Text.RegularExpressions;

let ordering = File.ReadAllLines("example1_1")
let updates = File.ReadAllLines("example1_2")
let lines2 = File.ReadAllLines("input2")

let example = File.ReadAllLines("example")
let example2 = File.ReadAllLines("example2")


let parseWithSplit (list: string array) (delimiter: string) =
    list
    |> List.ofArray
    |> List.map (fun e -> e.Split(delimiter) |> Array.map int |> List.ofArray )

let parseOrdering = parseWithSplit ordering "|"
let parseUpdates = parseWithSplit updates ","

let isUpdateSorted (rules: int list list) (updates: int list) =
    
    true

let sumMiddle (sortedLists: int list list) =
    sortedLists 
    |> List.fold (fun a i -> a + i.[(i.Length / 2 + 1)]) 0

let solution1 : int = 
    parseUpdates
    |> List.filter (isUpdateSorted parseOrdering)
    |> sumMiddle
    
let solution2 = 
    ignore

printfn "%d" solution1


    
