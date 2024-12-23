module day10

open System.IO  

let input = File.ReadAllText("input")
let example = File.ReadAllText("example")
let example1 = File.ReadAllText("example1")

type Stone = int64

let readInput (str: string) : Stone list =
    str.Split(" ") |> Seq.ofArray |> List.ofSeq |> List.map int64

let rule1 (stone: Stone) = [1L]
let rule2 (stone: Stone) =
    string stone 
    |> Seq.toList 
    |> Seq.splitInto 2 
    |> Seq.map (fun e -> e |> Seq.map string |> System.String.Concat |> int64)
    |> List.ofSeq
    
let rule3 stone = [stone*2024L]

let applyRule (stone: Stone)=
    match stone with
    | 0L -> rule1 stone
    | stoneNumber when (string stoneNumber).Length % 2 = 0 -> rule2 stone
    | _ -> rule3 stone
    
let blink =
    List.fold (fun acc e -> 
        let newElements = List.fold (fun acc e -> e :: acc) [] (applyRule e)
        newElements @ acc
    ) [] 
    >> List.rev

let solution1 (input: string) =
    let result = List.fold (fun acc _ -> blink acc) (readInput input) (List.init 25 id)  
    printfn $"result: %A{result} {result.Length}"
    
let test actual expected =
    if actual = expected
    then printfn $"PASSED {actual} = {expected}"
    else printfn $"FAILED {actual} = {expected}"

solution1 input
// test (rule1 0) [1]
// test (rule3 1) [2024]
// test (rule2 10) [1;0]
// test (rule2 99) [9;9]
// test (rule3 999) [2021976]

// test (applyRule 10) [1;0]
// test (applyRule 99) [9;9]
// test (applyRule 999) [2021976]
    