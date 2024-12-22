module day08

open System.IO  

let input = File.ReadAllText("input")
let example = File.ReadAllText("example")
let example1 = File.ReadAllText("example_1")

let expandFile (id: string) (c: string) = 
    Array.replicate (int c) id

let folder = fun (acc: string array, isFile, id) c -> 
    (Array.append acc 
        (expandFile (if isFile then string id else ".") c), 
        not isFile, 
        if isFile then id+1 else id)

let moveToLeftmostFreeSpace (map: string list) =
    printfn "%A" <| (map)
    let rec helper (map: string list) (leftP: int) (rightP: int) =
        if rightP <= leftP
        then map
        else match map[leftP] with
                | _ when map[rightP] = "." -> helper map leftP (rightP - 1)
                | "." -> 
                    helper (map |> List.mapi (fun i e -> 
                        match i with
                        | x when x = leftP -> map[rightP]
                        | x when x = rightP -> "."
                        | _ -> e)) (leftP + 1) (rightP - 1)
                | _ -> helper map (leftP + 1) rightP

    let a = helper map 0 (map.Length - 1)
    printfn "%A" <| a
    a

(*
    algoritm:
    om siffra, fortsätt söka tills samma siffra inte dyker upp längre
    då, kom ihåg hur många siffror vi stött på av samma typ
    börja söka från vänster (där vi är nu) om vi hittar lika många punkter i rad
    isåfall, ersätt punkterna med siffrorna och ta bort siffrorna från höger
    loop
*)

let findAvailableSpaceEndIndex (map: string array) stop (leftP: int) targetDotsCount: int option =
    let mutable currentDotsCount = 0
    let mutable result: int option = None
    let mutable index = leftP

    while index < stop && result.IsNone do
        match map[index] with
        | "." -> 
            currentDotsCount <- currentDotsCount + 1
            if currentDotsCount = targetDotsCount then
                result <- Some (index)
        | _ -> 
            currentDotsCount <- 0
        index <- index + 1

    result


    // if leftP >= stop
    // then None
    // else 
    //     if currentDotsCount = targetDotsCount
    //     then Some (leftP-1)
    //     else match map[leftP] with
    //                 | "." -> findAvailableSpaceEndIndex map stop (leftP+1) targetDotsCount (currentDotsCount+1)
    //                 | _ -> findAvailableSpaceEndIndex map stop (leftP+1) targetDotsCount 0

let findNextBlockStartIndex (map: string array) (rightP: int) (currentNumberCount: int): (int * int) option =
    let mutable index = rightP
    let mutable numberCount = currentNumberCount
    let mutable result: (int * int) option = None

    while index > 0 && result.IsNone do
        if numberCount = 0 then
            if map[index] = "." then
                numberCount <- 0
            else
                numberCount <- 1
        else
            if index < map.Length - 1 && map[index] <> map[index + 1] then
                result <- Some (index + 1, numberCount)
            else
                numberCount <- numberCount + 1
        index <- index - 1

    result

// let rec findNextBlockStartIndex (map: string array) (rightP: int) (currentNumberCount: int): (int*int) option =
//     if rightP = 0
//     then None
//     else
//         if currentNumberCount = 0 then
//             if map[rightP] = "." then findNextBlockStartIndex map (rightP-1) 0 else findNextBlockStartIndex map (rightP-1) 1 
//         else 
//             if rightP < map.Length && map[rightP] <> map[rightP+1]
//             then Some (rightP+1, currentNumberCount)
//             else findNextBlockStartIndex map (rightP-1) (currentNumberCount+1)

let cutBlock map availableSpaceEndIndex startIndex length: string array =
    map |> Array.mapi (fun i e -> 
        match i with
        | x when x > (availableSpaceEndIndex - length) && x <= availableSpaceEndIndex -> map[startIndex]
        | x when x >= startIndex && x < (startIndex + length) -> "."
        | _ -> e)

let rec helper (map: string array) (rightP: int) =
    printfn $"%d{rightP} / %d{map.Length}"
    match findNextBlockStartIndex map rightP 0 with
    | None -> (map, -1)
    | Some (blockStartIndex,blockLength) -> 
        match findAvailableSpaceEndIndex map rightP 0 blockLength with 
        | None -> (map, (blockStartIndex-1)) 
        | Some availableSpaceEndIndex -> 
            (cutBlock map availableSpaceEndIndex blockStartIndex blockLength, blockStartIndex-1)

let moveToLeftmostFreeSpace2 (map: string array) =
    let mutable rightP = map.Length - 1
    let mutable mapp = map 
    while rightP > 0 do
        let res = helper mapp rightP
        mapp <- fst res
        rightP <- snd res
        
    // too high, fick 15540538497475
    // varför???? :(

    printfn "%A" <| (String.concat "" mapp)
    mapp   
    // a

let read (input: string) = 
    input
    |> Seq.map string |> Seq.toArray
    |> Array.fold folder (Array.empty, true, 0)
    |> fun (a,_,_) -> a

let computeChecksum (lst: string array): int64 = 
    lst 
    |> Array.fold (fun (acc, i) e -> ((if e <> "." then (i * (int64 e)) else 0L) + acc, i+1L)) (0L,0L)
    |> fst

let solution1 (input: string) : int64 = 
    read input
    |> moveToLeftmostFreeSpace2
    |> computeChecksum

printfn "%d" <| (solution1 input) 

    
