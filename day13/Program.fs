module day13

open System.IO  
open System.Text.RegularExpressions

let input = File.ReadAllLines("input") |> List.ofArray
let example = File.ReadAllLines("example") |> List.ofArray
let example1 = File.ReadAllLines("example1") |> List.ofArray

let getCoords regex input  =
    match Regex.Match(input, regex) with
    | x when x.Success ->
        x.Groups
        |> Seq.toList
        |> (fun e -> (e[1].Value |> int, e[2].Value |> int))
    | _ -> failwith "lol"

let findSmallestPushes (ax1,ay1) (bx1,by1) (px1,py1) : (int*int) option=
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
    
    let ax, ay, bx, by, px, py = float ax1,float ay1,float bx1,float by1,float px1,float py1
    
    let b = ((ay*px)-(py*ax)) / ((bx*ay) - (by*ax))
    let a = (px - (bx*b)) / ax
    if floor a = a && floor b = b
    then Some (int a, int b)
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
            | Some (a,b) -> (a*3) + b
            | None -> 0)
    |> List.sum

printfn $"%A{solution1 input}"