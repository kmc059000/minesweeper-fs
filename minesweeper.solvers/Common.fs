module Common

open Coordinates
open Cells
open Games

    module Solvers = 
        type HiddenCell = { Coords: Coordinate; }
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

        module Solution =
            
            let withProbability p s = { s with LastProbability = p }
            let withSweepCounts perfect imperfect s = 
                {s with 
                    PerfectSweeps = s.PerfectSweeps + perfect;
                    ImperfectSweeps = s.ImperfectSweeps + imperfect
                }

            let ofGame (game:Game) =
                let getSolutionCell (c:Cell) = 
                    match c.State with
                    | CellState.Hidden -> (c.Coords.Index, Hidden { Coords = c.Coords; })
                    | CellState.Exposed -> (c.Coords.Index, Exposed { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })
                    | CellState.Flagged -> (c.Coords.Index, Flagged { Coords = c.Coords; SurroundingCount = c.SurroundingCount.Value; })

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

        let solve solver (game:Game) =
            game |> Solution.ofGame |> solver
    

module Utilities =
    open Commands.Sweep
    open Commands.Flag
    open Solvers

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

    let solutionCellsOfSeq solution = solution.Cells |> Map.toSeq |> Seq.map snd

    let getFlaggedCells = getCellsOfType getFlaggedCell
    let getHiddenCells = getCellsOfType getHiddenCell
    let getExposedCells = getCellsOfType getExposedCell

    let getNeighborsOfType typeMatcher solution coords =
        Coordinates.getValidSurroundingIndexes coords
        |> Seq.map (fun n -> solution.Cells.[n.Index])
        |> Seq.choose typeMatcher
                
    let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
    let getHiddenNeighbors = getNeighborsOfType getHiddenCell
    let getExposedNeighbors = getNeighborsOfType getExposedCell

    let rec sweepAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            game
            |> sweep x.Coords.X x.Coords.Y
            |> sweepAll xs
        
    let rec flagAll (cells:HiddenCell list) game =
        match cells with
        | [] -> game
        | x::xs ->
            game |> flag x.Coords.X x.Coords.Y
            |> flagAll xs