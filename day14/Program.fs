module day13

open System.IO  
open System.Text.RegularExpressions
open System;

let input = File.ReadAllText("input")
let example = File.ReadAllText("example")

type Position = int*int
type Velocity = int*int
type Robot = Position * Velocity
type Dimensions = int*int

let getCoords regex input : Robot list =
    Regex.Matches(input, regex)
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun m ->
        match m with
        | x when x.Success ->
            x.Groups
            |> Seq.toList
            |> (fun e -> ((e[1].Value |> int, e[2].Value |> int),
                        (e[3].Value |> int, e[4].Value |> int)))
        | _ -> failwith "lol"
    )

let readData = getCoords @"p\=([-]?\d+),([-]?\d+)\sv\=([-]?\d+),([-]?\d+)"

let xMid width = (float width / 2.0) |> floor |> int
let yMid height = (float height / 2.0) |> floor |> int

let partition f g list =
    (List.filter f list, List.filter g list)

let calculateSafetyScore (width,height) (robots: Robot list) =
    robots
    |> partition (fun ((x,_),_) -> x < (xMid width)) (fun ((x,_),_) -> x > (xMid width))
    |> fun (left,right) -> (
        partition (fun ((_,y),_) -> y < (yMid height)) (fun ((_,y),_) -> y > (yMid height)) left,
        partition (fun ((_,y),_) -> y < (yMid height)) (fun ((_,y),_) -> y > (yMid height)) right
        )
    |> fun ((a,b),(c,d)) -> [a;b;c;d]
    |> List.map (fun q -> (List.length q |> fun i -> if i = 0 then 1 else i))
    |> List.fold (fun acc q -> acc * q) 1
    
let calculateSafetyScore2 (width,height) (robots: Robot list) =
    robots
    |> partition (fun ((x,_),_) -> x < (xMid width)) (fun ((x,_),_) -> x > (xMid width))
    |> fun (left,right) -> (
        partition (fun ((_,y),_) -> y < (yMid height)) (fun ((_,y),_) -> y > (yMid height)) left,
        partition (fun ((_,y),_) -> y < (yMid height)) (fun ((_,y),_) -> y > (yMid height)) right
        )
    |> fun ((a,b),(c,d)) -> [a;b;c;d]
    |> List.map (fun q -> (List.length q |> fun i -> if i = 0 then 1 else i))

let loopAround pos limit =
    match pos with
    | p when p >= limit -> p % limit
    | p when p < 0 -> limit - (abs p)
    | _ -> pos

let tick (width,height) (((x,y),(vx,vy)): Robot) =
    ((loopAround (x+vx) width, loopAround (y+vy) height),(vx,vy))
   
let drawPositions (width, height) robots =
    for y in (List.init height id) do
        for x in (List.init width id) do
            let cnt = robots |> List.sumBy (fun ((rx,ry),_) -> if ((rx = x) && (ry = y)) then 1 else 0)
            if cnt > 0
            then printf "%d" cnt
            else printf "."
        printf "\n"
    
let tickAll times dimensions (robots: Robot list) =
    let topFrame = List.init 101 (fun i -> (i+1,47))
    
    List.fold
        (fun acc i ->
            let res = acc |> List.map (tick dimensions)
            if i % 1000 = 0 then printfn "%d" i else ()
            
            if (topFrame |> List.filter (fun (ex,ey) ->
                match List.tryFind (fun ((x,y),_) -> ex = x && ey = y) res with
                | Some x -> true
                | None -> false)) |> List.length > 30
            then 
                drawPositions dimensions res
                printfn "%d" i
            else printf ""
            res
            )
        robots
        (List.init times (fun i -> i+1))
                   


let solution1 (input: string) dimensions =
    let robots = readData input |> tickAll 100000 dimensions
    0

// printfn $"%A{solution1 example (11,7)}"
printfn $"%d{solution1 input (101,103)}"