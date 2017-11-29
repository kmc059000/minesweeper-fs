﻿module Common

open Coordinates
open Cells
open Games

type HiddenCell = { Coords: Coordinate2; TotalNeighbors: int; }
type ExposedCell = { Coords: Coordinate2; SurroundingCount: int; }

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

module SolutionCells =
    let getXY gameSize c =
        let c2 = 
            match c with
            | Hidden c -> c.Coords
            | Exposed c -> c.Coords
            | Flagged c -> c.Coords
        Coordinates2.toXY c2 gameSize

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
        Coordinates2.getValidSurroundingIndexes solution.Game.GameSize coords
        |> Seq.map (fun idx -> solution.Cells.[idx])
        |> Seq.choose typeMatcher
                
    let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
    let getHiddenNeighbors = getNeighborsOfType getHiddenCell
    let getExposedNeighbors = getNeighborsOfType getExposedCell

module Cell =
    let private callWithCoords fn solution cell = fn solution cell.Coords
    let private getFlaggedNeighbors = callWithCoords Coordinate.getFlaggedNeighbors
    let private getHiddenNeighbors = callWithCoords Coordinate.getHiddenNeighbors

    let private getTypeCount fn solution cell = cell |> fn solution |> Seq.length |> float
    let getFlaggedCount = getTypeCount getFlaggedNeighbors
    let getHiddenCount = getTypeCount getHiddenNeighbors


     
module Solution =
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
            let cellState = Game.getCellStateFromCell c game
            let index = Cells.getIndex c
            match cellState with
            | CellState.Hidden -> (index, Hidden { Coords = c.Coords; TotalNeighbors = c.TotalNeighbors; })
            | CellState.Exposed -> (index, Exposed { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })
            | CellState.Flagged -> (index, Flagged { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })

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
            let cellState = Game.getCellStateFromCell cell solution.Game
            match cellState with 
            | CellState.Hidden -> { acc with Hidden = acc.Hidden + 1 }
            | CellState.Exposed -> { acc with Exposed = acc.Exposed + 1 }
            | CellState.Flagged -> { acc with Flagged = acc.Flagged + 1 }
        ) { Hidden = 0;  Exposed = 0; Flagged = 0; }


module Game =
    open Commands.Sweep
    open Commands.Flag

    let rec sweepAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            let x', y', _ = SolutionCells.getXY game.GameSize (Hidden x)
            game
            |> sweep x' y'
            |> sweepAll xs
        
    let rec flagAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            let x', y', _ = SolutionCells.getXY game.GameSize (Hidden x)
            game 
            |> flag x' y'
            |> flagAll xs

    let getRandom (rand:System.Random) xs =
        let idx = rand.Next(List.length xs)
        List.item idx xs

    let filterToFewestNeighbors cells =
        cells 
        |> Seq.groupBy (fun c -> c.TotalNeighbors)
        |> Seq.minBy (fun (k,_) -> k)
        |> snd

    let sweepRandom cells (rand:System.Random) game =
        let bestCells = cells |> filterToFewestNeighbors |> Seq.toList
        let cell = bestCells |> getRandom rand
        sweepAll [cell] game

    let solve solver (game:Game) =
        game |> Solution.ofGame |> solver
