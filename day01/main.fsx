open System.IO  
open System.Collections.Generic

let lines = File.ReadAllLines("input") |> Array.toList

let example = File.ReadAllLines("example") |> Array.toList

let readInput (arr: string list) = 
    arr |> List.map (fun s -> s.Split("   ") |> Array.toList)

let solution1 : int = 
    let lists = readInput lines
    let left = lists |> List.map (fun l -> l.[0])
    let right = lists |> List.map (fun l -> l.[1])

    List.zip (left |> List.sort) (right |> List.sort)
    |> List.map (fun (a,b) -> System.Math.Abs(int a - int b))
    |> List.sum

let counts list = 
    let updateCount map item = 
        match Map.tryFind item map with
        | Some cnt -> Map.add item (cnt + 1) map
        | None -> Map.add item 1 map

    List.fold updateCount Map.empty list

let solution2 : int = 
    let lists = readInput lines
    let left = lists |> List.map (fun l -> int l.[0])
    let right = lists |> List.map (fun l -> int l.[1])

    let cnt = counts right

    left |> List.map (fun a -> 
        match Map.tryFind a cnt with 
        | Some x -> a * x
        | None -> 0)
    |> List.sum

printfn "%d" solution1
printfn "%d" solution2


    
