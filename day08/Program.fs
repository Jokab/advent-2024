module day08

open System.IO  

let input = File.ReadAllLines("input")
                |> List.ofArray 
                |> List.map Seq.toList
let example = File.ReadAllLines("example")
                |> List.ofArray 
                |> List.map Seq.toList
let exampleMine = File.ReadAllLines("example_1") 
                |> List.ofArray 
                |> List.map Seq.toList

type Point = int*int
type Grid = char list list
type Antenna = char
type AntennaPosition = Point
type AntinodePosition = Point

type FoundAntinodes = Set<AntinodePosition>
type AntennaPositionsByType = Map<Antenna, AntennaPosition list>

let parseGrid: char list list -> Grid = fun input -> input
// type FromPoint = Grid -> Point -> Antenna option

type GroupAntennasByType = Grid -> AntennaPositionsByType
let groupAntennasByType : GroupAntennasByType = fun grid -> 
    grid
    |> List.mapi (fun rowIndex row -> 
        row 
        |> List.mapi (fun colIndex char -> (char, (colIndex, rowIndex)))
    )
    |> List.concat
    |> List.filter (fun (char, _) -> System.Char.IsLetterOrDigit char)
    |> List.groupBy fst
    |> List.map (fun (key, values) -> key, List.map snd values)
    |> Map.ofList

let positionInsideGrid : Grid -> AntennaPosition -> bool =
    fun grid (x,y) -> y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length

(*
    y1 = k*x1 + m
    y2 = k*x2 + m
    y2 - y1 = kx2 + m - (kx1 + m) = kx2 + m - kx1 - m = kx2 - kx1 = k(x2 - x1)
    k = y2 - y1 / x2 - x1
    m = y1 - k*x1

    c^2 = sqrt(a^2 + b^2)
*)

let calculateAntinodesFromPosition: Grid -> AntennaPosition * AntennaPosition -> Set<AntinodePosition> 
    = fun grid ((ix1, iy1), (ix2, iy2)) ->
        let x1 = float ix1
        let x2 = float ix2
        let y1 = float iy1
        let y2 = float iy2
        // let dy = abs(y2 - y1)
        let dx = abs(x2 - x1)
        let k = float (y2 - y1) / (x2 - x1)
        let m = y1 - (k*x1)
        let anti1 = ((min x1 x2) - dx |> round |> int, k * ((min x1 x2) - dx) + m |> round |> int)
        let anti2 = ((max x1 x2) + dx |> round |> int, k * ((max x1 x2) + dx) + m |> round |> int)
        set [anti1; anti2] |> Set.filter (positionInsideGrid grid)
    
(*
    vad innebär det att k=1/3?
    för varje 3x ökar y med 1
    y ges av  
    
*)
    
(*  Hitta alla som är på en rak linje?
    givet (1,2)
    borde ekvationen bli y = x+1? -> y = 0+1 = 1 -> (0,1), (1,2), (2,3), (3,4)
    eller ekvationen y = 0x + 2 för en rak linje -> (0,2), (1,2), (2,2), (3,2)
    om man tar fram alla från antennaPositions som är i listan ovan
    givet då (1,2) och (2,2)
    -> avståndet i planet är 1 (2-1) -> antinodes på (0,2) och (3,2)
*)
let findAntinodesOfType : Grid -> Antenna * AntennaPosition list -> Set<AntinodePosition>
    = fun grid (antenna, antennaPositions) -> 
        List.allPairs antennaPositions antennaPositions
        |> List.filter (fun (e1, e2) -> fst e1 <> fst e2 || snd e1 <> snd e2)
        |> List.fold (fun acc pair -> Set.union (calculateAntinodesFromPosition grid pair) acc) (set [])

let findAllAntinodes : Grid -> AntennaPositionsByType -> FoundAntinodes = fun grid  -> 
    Map.fold (fun acc key entry -> Set.union (findAntinodesOfType grid (key, entry)) acc) (set []) 

let countAntinodes: FoundAntinodes -> int = fun foundAntinodes -> foundAntinodes.Count


let solution1 (lines: char list list) : int = 
    let grid = parseGrid lines
    grid
    |> groupAntennasByType
    |> findAllAntinodes grid
    |> countAntinodes
    

printfn "%d %d" (solution1 exampleMine) 2
printfn "%d %d" (solution1 example) 14
printfn "%d" (solution1 input)
// printfn "%b" <| (((2,1),(2,2)) |> fun (e1, e2) -> fst e1 <> fst e2 && snd e1 <> snd e2)


// printfn "%b" <| (solution1 example = 14)
// printfn "%A" <| (solution1 (List.ofArray example))


    
