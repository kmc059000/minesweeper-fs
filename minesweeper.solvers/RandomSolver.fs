module RandomSolver

open Minesweeper
open Commands.Sweep
open Commands.Flag
open Common

    module private Solvers =
        let rand = new System.Random()
        let rec solveRandomly (solution:Solution) = 
            match solution.SolutionState with
            | Win | Dead -> solution
            | _ -> 
                let sweepableCells = getUnsolvedCells solution |> Array.ofSeq
                let cellToSweep = sweepableCells.[rand.Next(sweepableCells.Length)]
                let game = sweep solution.Game cellToSweep.Coords.X cellToSweep.Coords.Y
                let newSolution = getSolutionFromGame game
                solveRandomly newSolution


let randomSolver = solve Solvers.solveRandomly
