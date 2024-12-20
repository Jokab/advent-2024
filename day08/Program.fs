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
    
let calculateAntinodesFromPosition_Part1: Grid -> AntennaPosition * AntennaPosition -> Set<AntinodePosition> 
    = fun grid ((ix1, iy1), (ix2, iy2)) ->
        let x1, x2, y1, y2 = (float ix1, float ix2, float iy1, float iy2)
        let dx = abs(x2 - x1)
        let k = float (y2 - y1) / (x2 - x1)
        let m = y1 - (k*x1)
        let anti1 = ((min x1 x2) - dx |> round |> int, k * ((min x1 x2) - dx) + m |> round |> int)
        let anti2 = ((max x1 x2) + dx |> round |> int, k * ((max x1 x2) + dx) + m |> round |> int)
        set [anti1; anti2] |> Set.filter (positionInsideGrid grid)

let calculateAntinodesFromPosition_Part2: Grid -> AntennaPosition * AntennaPosition -> Set<AntinodePosition> 
    = fun grid ((ix1, iy1), (ix2, iy2)) ->
        let x1, x2, y1, y2 = (float ix1, float ix2, float iy1, float iy2)
        let dx = abs(x2 - x1)
        let k = float (y2 - y1) / (x2 - x1)
        let m = y1 - (k*x1)
           
        let rec helper nextPos acc (x: float, y: float): AntinodePosition list =
            if positionInsideGrid grid (x |> round |> int, y |> round |> int)
            then helper nextPos ((x,y) :: acc) (nextPos (x,y))
            else acc |> List.map (fun (x,y) -> (int x, int y))
        
        Set.union
            (set 
                (helper
                    (fun (x,_) -> (x - dx |> round, k * (x - dx) + m |> round))
                    []
                    ((min x1 x2), k * (min x1 x2) + m |> round)
                ))
            (set 
                (helper
                    (fun (x,_) -> (x + dx |> round, k * (x + dx) + m |> round))
                    []
                    ((max x1 x2), k * (max x1 x2) + m |> round)
                ))
        |> Set.filter (positionInsideGrid grid)
    
let findAntinodesOfType : Grid -> Antenna * AntennaPosition list -> Set<AntinodePosition>
    = fun grid (antenna, antennaPositions) -> 
        List.allPairs antennaPositions antennaPositions
        |> List.filter (fun (e1, e2) -> fst e1 <> fst e2 || snd e1 <> snd e2)
        |> List.fold (fun acc pair -> Set.union (calculateAntinodesFromPosition_Part1 grid pair) acc) (set [])

let findAllAntinodes : Grid -> AntennaPositionsByType -> FoundAntinodes = fun grid  -> 
    Map.fold (fun acc key entry -> Set.union (findAntinodesOfType grid (key, entry)) acc) (set []) 

let solution1 (lines: char list list) : int = 
    let grid = parseGrid lines
    grid
    |> groupAntennasByType
    |> findAllAntinodes grid
    |> _.Count
    
let solution2 (lines: char list list) : int = 
    let grid = parseGrid lines
    grid
    |> groupAntennasByType
    |> findAllAntinodes grid
    |> _.Count
    
printfn "%d %d" (solution1 exampleMine) 2
printfn "%d %d" (solution1 example) 14
printfn "%d" (solution1 input)


    
