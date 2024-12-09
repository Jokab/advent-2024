open System.IO  
open System.Text.RegularExpressions;

let lines = File.ReadAllLines("input")
let lines2 = File.ReadAllLines("input2")

let example = File.ReadAllLines("example")
let example2 = File.ReadAllLines("example2")

let readInput (arr: string array) = 
    arr.[0]

let parseMul (muls: Match) : int =
    let pattern = @"mul\((\d+),(\d+)\)"
    let nums = Regex.Match(muls.Groups.[0].Value, pattern, RegexOptions.IgnoreCase)
    int nums.Groups.[1].Value * int nums.Groups.[2].Value

let parseMul2 muls : int =
    let pattern = @"mul\((\d+),(\d+)\)"
    let nums = Regex.Match(muls, pattern, RegexOptions.IgnoreCase)
    int nums.Groups.[1].Value * int nums.Groups.[2].Value

let solution1 : int = 
    let input = readInput lines
    let pattern = @"mul\(\d+,\d+\)"
    Regex.Matches(input, pattern, RegexOptions.IgnoreCase)
        |> List.ofSeq
        |> List.map parseMul 
        |> Seq.sum

let solution2 : int = 
    let input = readInput lines2
    let pattern = @"(mul\(\d+,\d+\))|((do\(\)))|((don't\(\)))"
    Regex.Matches(input, pattern, RegexOptions.IgnoreCase)
        |> List.ofSeq
        |> List.fold (fun (acc, doMul) m -> 
            match m.Value with 
            | "don't()" -> (acc, false)
            | "do()" -> (acc, true)
            | mulMatch when doMul && mulMatch <> "" -> (acc + parseMul2 mulMatch, doMul)
            | _ -> (acc, doMul)
            ) (0, true)
        |> fst

printfn "%d" solution1
printfn "%d" solution2


    
