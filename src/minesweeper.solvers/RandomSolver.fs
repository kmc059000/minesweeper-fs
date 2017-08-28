module RandomSolver

open Commands.Sweep
open Common

let private rand = new System.Random()

let rec solve solution = 
    match solution.SolutionState with
    | Win | Dead -> solution
    | _ -> 
        let sweepableCells = Coordinate.getHiddenCells solution |> Array.ofSeq
        let cellToSweep = sweepableCells.[rand.Next(sweepableCells.Length)]
        solution.Game 
        |> sweep cellToSweep.Coords.X cellToSweep.Coords.Y
        |> Solution.ofGame
        |> solve


