module day10

open System.IO  
open System.Collections.Generic

let input = File.ReadAllText("input")
let example = File.ReadAllText("example")
let example1 = File.ReadAllText("example1")


let readInput (str: string) : int64 list =
    str.Split(" ") |> Seq.ofArray |> List.ofSeq |> List.map int64

let rule1 (stone: int64) = 1L
let rule2 (stone: int64) =
    let digits = int(log10(float stone)) + 1
    let mid = digits / 2                      
    let divisor = pown 10L mid                
    (stone / divisor, stone % divisor)
let rule3 stone = stone*2024L

type RuleResult =
    | Int64 of int64
    | Tup of int64*int64

let applyRule (stone: int64): RuleResult =
    match stone with
    | 0L -> Int64 (rule1 stone)
    | _ when (int(log10(float stone)) + 1) % 2 = 0 -> Tup (rule2 stone)
    | _ -> Int64 (rule3 stone)
    
let blink =
    let memo = Dictionary<_, _>()
    let rec blink' numBlinks lst =
        match numBlinks with
        | 0 -> 1L
        | _ ->
            if memo.ContainsKey((numBlinks, lst)) then
                memo.[(numBlinks, lst)]
            else
                let result =
                    match applyRule lst with
                    | Tup (x, y) -> (blink' (numBlinks - 1) x) + (blink' (numBlinks - 1) y)
                    | Int64 x -> blink' (numBlinks - 1) x
                memo.[(numBlinks, lst)] <- result
                result
    blink'

let solution1 (input: string) =
    let data = readInput input
    let result = List.fold (fun acc stone -> acc + blink 75 stone) 0L data
    printfn $"Result: {result}"

solution1 input