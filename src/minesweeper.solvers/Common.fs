module Common

open Coordinates
open Cells
open Games

type HiddenCell = { Coords: Coordinate; TotalNeighbors: int; }
type ExposedCell = { Coords: Coordinate; SurroundingCount: int; }

type VisibleCell = 
    | Hidden of HiddenCell
    | Exposed of ExposedCell
    | Flagged of ExposedCell

type CellCounts = { Hidden: int; Exposed: int; Flagged: int; }

//restricted view of the game for the solver
type SolutionState = Unsolved | Win | Dead
type Solution = {
    Game : Game;
    Cells: Map<int, VisibleCell>;
    SolutionState: SolutionState;
    LastProbability: float option
    PerfectSweeps: int;
    ImperfectSweeps: int;
}

type Command = Flag of (Coordinate Set) | Sweep of (Coordinate Set) | SweepRandom of (Coordinate Set)

module Coordinate =
    //i wish i could find a way to write these 3 functions as the same generic function
    let getExposedCell cell =
        match cell with 
        | Exposed e -> Some e
        | _ -> None
        
    let getFlaggedCell cell =
        match cell with 
        | Flagged e -> Some e
        | _ -> None

    let getHiddenCell cell =
        match cell with 
        | Hidden e -> Some e
        | _ -> None

    let getCellsOfType typeMatcher solution =
        solution.Cells |> Map.toSeq |> Seq.map snd |> Seq.choose typeMatcher

    let getFlaggedCells = getCellsOfType getFlaggedCell
    let getHiddenCells = getCellsOfType getHiddenCell
    let getExposedCells = getCellsOfType getExposedCell

    let getNeighborsOfType typeMatcher solution coords =
        Coordinates.getValidSurroundingIndexes coords
        |> Seq.map (fun idx -> solution.Cells.[idx])
        |> Seq.choose typeMatcher
                
    let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
    let getHiddenNeighbors = getNeighborsOfType getHiddenCell
    let getExposedNeighbors = getNeighborsOfType getExposedCell

    let getNeighbors solution coords =
        Coordinates.getValidSurroundingIndexes coords
        |> Seq.map (fun idx -> solution.Cells.[idx])

    let ofHiddenCell (c:HiddenCell) = c.Coords

module Cell =
    let private callWithCoords fn solution cell = fn solution cell.Coords
    let private getFlaggedNeighbors = callWithCoords Coordinate.getFlaggedNeighbors
    let private getHiddenNeighbors = callWithCoords Coordinate.getHiddenNeighbors

    let private getTypeCount fn solution cell = cell |> fn solution |> Seq.length |> float
    let getFlaggedCount = getTypeCount getFlaggedNeighbors
    let getHiddenCount = getTypeCount getHiddenNeighbors


     
module Solution =
    open System

    let cellsToSeq s = s.Cells |> Map.toSeq |> Seq.map snd

    let private getCount whereCellTypeMatches solution = 
        solution 
        |> cellsToSeq 
        |> Seq.choose whereCellTypeMatches
        |> Seq.length

    let flaggedCount = getCount Coordinate.getFlaggedCell
    let hiddenCount = getCount Coordinate.getHiddenCell

    let withProbability p s = { s with LastProbability = (Some p) }

    let withSweepCounts previousGameState perfect imperfect s = 
        match previousGameState with
        | GameState.Start -> { s with PerfectSweeps = 1; } 
        | _ -> 
            { s with 
                PerfectSweeps = s.PerfectSweeps + perfect;
                ImperfectSweeps = s.ImperfectSweeps + imperfect
            }

    let ofGame (game:Game) =
        let getSolutionCell (c:Cell) = 
            match c.State with
            | CellState.Hidden -> (c.Coords.Index, Hidden { Coords = c.Coords; TotalNeighbors = c.TotalNeighbors;
            })
            | CellState.Exposed -> (c.Coords.Index, Exposed { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })
            | CellState.Flagged -> 
                match c.SurroundingCount with
                | Some count -> c.Coords.Index, Flagged { Coords = c.Coords; SurroundingCount = count }
                | None -> failwith "Flagged a cell that is not a mine"

        let cells = game.Cells |> Map.toSeq |> Seq.map snd |> Seq.map getSolutionCell |> Map.ofSeq
        let state = 
            match game.State with
            | GameState.Win -> Win
            | GameState.Dead -> Dead
            | GameState.Start | GameState.Playing | _ -> Unsolved
        { Game = game; Cells = cells; SolutionState = state; LastProbability = None; PerfectSweeps = 0; ImperfectSweeps = 0; }

    let cellCounts solution =
        solution.Game.Cells 
        |> Map.toSeq 
        |> Seq.map snd 
        |> Seq.fold (fun acc cell -> 
            match cell.State with 
            | CellState.Hidden -> { acc with Hidden = acc.Hidden + 1 }
            | CellState.Exposed -> { acc with Exposed = acc.Exposed + 1 }
            | CellState.Flagged -> { acc with Flagged = acc.Flagged + 1 }
        ) { Hidden = 0;  Exposed = 0; Flagged = 0; }


module Game =
    open Commands.Sweep
    open Commands.Flag

    let private cellToCoords cells = cells |> Seq.map Coordinate.ofHiddenCell |> Seq.toList

    let rec sweepAllCoords coords game =
        match coords with
        | [] -> game
        | x::xs ->
            game
            |> sweep x.X x.Y
            |> sweepAllCoords xs

    let sweepAll (cells:HiddenCell list) game =
        sweepAllCoords (cellToCoords cells) game
        
    let rec flagAllCoords coords game =
        match coords with
        | [] -> game
        | x::xs ->
            game 
            |> flag x.X x.Y
            |> flagAllCoords xs

    let flagAll (cells:HiddenCell list) game =
        flagAllCoords (cellToCoords cells) game

    let getRandom (rand:System.Random) xs =
        let idx = rand.Next(List.length xs)
        List.item idx xs

    let filterToFewestNeighbors cells =
        cells 
        |> Seq.groupBy (fun c -> c.TotalNeighbors)
        |> Seq.minBy (fun (k,_) -> k)
        |> snd

    let sweepRandomCoord coords (rand:System.Random) game =
        let bestCoords = coords |> Seq.toList
        let coord = bestCoords |> getRandom rand
        sweepAllCoords [coord] game

    let sweepRandom cells (rand:System.Random) game =
        let coords = cells |> cellToCoords 
        sweepRandomCoord coords rand game

    let solve solver (game:Game) =
        game |> Solution.ofGame |> solver
