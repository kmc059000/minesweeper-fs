module RandomSolver

open Commands.Sweep
open Common
open Coordinates

let private rand = new System.Random()

let rec solve solution = 
    match solution.SolutionState with
    | Win | Dead -> solution
    | _ -> 
        let sweepableCells = Coordinate.getHiddenCells solution |> Array.ofSeq
        let cellToSweep = sweepableCells.[rand.Next(sweepableCells.Length)]
        let x', y', _ = SolutionCells.getXY solution.Game.GameSize (Hidden cellToSweep)
        solution.Game 
        |> sweep x' y'
        |> Solution.ofGame
        |> solve


