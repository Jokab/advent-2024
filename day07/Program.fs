module day07

open System.IO  
open System.Text.RegularExpressions;

let lines = File.ReadAllLines("input")
let lines2 = File.ReadAllLines("input2")

let example = File.ReadAllLines("example")
let example2 = File.ReadAllLines("example2")

let parseLine (line:string) =
    line.Split(":")
    |> List.ofArray
    |> fun arr -> 
        match arr with
            | [sum;rest] -> 
                int sum :: (
                    rest.Trim().Split(" ") 
                    |> List.ofArray 
                    |> List.map int
                )
            | _ -> [0;0]

///
/// 3267: 81 40 27
/// 3267 - 27 = 3240
/// 3240 - 40 != 81 --> testa div
/// 3240 / 40 = 81  nice!
/// 
/// 
/// 292: 11 6 16 20
/// 292 - 20 = 272
/// 272 - 16 = 

type Sign = Sub | Div
let toggleSign sign = if sign = Sub then Div else Sub

let calcWithFirstInList (sign: Sign) (acc: int) (list: int list) = 
    match sign with
    | Sub -> (-) acc list.[0]
    | Div -> (/) acc list.[0]

let canCombine (sum: int) (nums: int list) =
    let rec helper acc (list: int list) (sign: Sign) = 
        match list.Length with
        | 1 -> acc = List.last list
        | _ -> match calcWithFirstInList sign acc list with
                | x when x > sum -> false
                | x -> match helper x (list[1..]) sign with
                        | true -> true 
                        | false -> helper x (list[1..]) (toggleSign sign)
    
    // Reverse so we are always popping first elem
    helper sum (List.rev nums) (Sub)

let solution1 (lines: string list) : int = 
    lines
    |> List.map parseLine
    |> List.filter (fun lst -> canCombine lst.[0] lst.[1..])
    |> List.fold (fun a e -> a + e.[0]) 0
    

printfn "%A" <| (solution1 (["190: 19 10"]))
// printfn "%A" <| (solution1 (List.ofArray example))
// printfn "%d" solution2


    
