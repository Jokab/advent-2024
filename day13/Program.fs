module day13

open System.IO  
open System.Text.RegularExpressions
open System;

let input = File.ReadAllLines("input") |> List.ofArray
let example = File.ReadAllLines("example") |> List.ofArray
let example1 = File.ReadAllLines("example1") |> List.ofArray
let example2 = File.ReadAllLines("example2") |> List.ofArray

let getCoords regex input  =
    match Regex.Match(input, regex) with
    | x when x.Success ->
        x.Groups
        |> Seq.toList
        |> (fun e -> (
            e[1].Value |> double,
            e[2].Value |> double
        ))
    | _ -> failwith "lol"

let findSmallestPushes (ax1,ay1) (bx1,by1) (px1,py1)=
    (*
        ax*a + bx*b = px
            a = px - bx*b / ax
    
        ay*a + by*b = py
            ay(px - bx*b / ax) + by*b = py
            (ay*px - bx*b*ay) / ax + by*b = py
            ay*px - bx*b*ay + by*b*ax = py*ax
            ay*px - b(by*ax - bx*ay) = py*ax
            b = (ay*px - py*ax) / (bx*ay - by*ax)
    
            - b(by*ax - bx*ay) = py*ax - ay*px
            b = (py*ax - ay*px) / (by*ax + bx*ay) 
    *)
    // let b = (ay*px - py*ax) / (bx*ay + by*ax)
    // let b = ((ay*px)-(py*ax)) / ((by*ax) - (bx*ay))
    
    let ax, ay, bx, by, px, py =  ax1, ay1, bx1, by1,(px1+10000000000000.0),(py1+10000000000000.0)
    
    let b = ((ay*px)-(py*ax)) / ((bx*ay) - (by*ax))
    let a = (px - (bx*b)) / ax
    if Math.Floor a = a && Math.Floor b = b
    then Some (double a, double b)
    else None
    
let buttonA = getCoords @"Button A: X\+(.+), Y\+(.+)"
let buttonB = getCoords @"Button B: X\+(.+), Y\+(.+)"
let prize = getCoords @"Prize: X\=(.+), Y\=(.+)"
    
let readInput (input: string list) =
    input
    |> List.chunkBySize 4
    |> List.map (List.take 3)
    
let solution1 (input: string list) =
    readInput input
    |> List.map (fun e ->
        [buttonA; buttonB; prize]
        |> List.mapi (fun i f -> f e[i])
        |> fun [buttonA; buttonB; prize] ->
            match findSmallestPushes buttonA buttonB prize with
            | Some (a,b) -> (a*3.0) + b
            | None -> 0.0)
    |> List.sum

printfn $"%f{solution1 input}"