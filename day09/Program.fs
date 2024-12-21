module day08

open System.IO  

let input = File.ReadAllText("input")
let example = File.ReadAllText("example")
let example1 = File.ReadAllText("example_1")

let expandFile (id: string) (c: string) = 
    List.replicate (int c) id

let folder = fun (acc, isFile, id) c -> 
    (acc @ 
        (expandFile (if isFile then string id else ".") c), 
        not isFile, 
        if isFile then id+1 else id)

let moveToLeftmostFreeSpace (map: string list) =
    printfn "%s" <| (String.concat "" map)
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

let read (input: string) = 
    input
    |> Seq.map string |> Seq.toList
    |> List.fold folder ([], true, 0)
    |> fun (a,_,_) -> a

let computeChecksum (lst: string list): int64 = 
    lst 
    |> List.fold (fun (acc, i) e -> ((if e <> "." then (i * (int64 e)) else 0L) + acc, i+1L)) (0L,0L)
    |> fst

let solution1 (input: string) : int64 = 
    read input
    |> moveToLeftmostFreeSpace
    |> computeChecksum

printfn "%d" <| (solution1 input) 

    
