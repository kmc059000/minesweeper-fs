module RandomSolver

open Commands.Sweep
open Common

    module private Solvers =
        let rand = new System.Random()
        let rec solveRandomly (solution:Solution) = 
            match solution.SolutionState with
            | Win | Dead -> solution
            | _ -> 
                let sweepableCells = Coordinate.getHiddenCells solution |> Array.ofSeq
                let cellToSweep = sweepableCells.[rand.Next(sweepableCells.Length)]
                let game = solution.Game |> sweep cellToSweep.Coords.X cellToSweep.Coords.Y
                game
                |> Solution.ofGame
                |> solveRandomly


let randomSolver = Game.solve Solvers.solveRandomly
