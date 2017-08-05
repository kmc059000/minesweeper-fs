module RandomSolver

open Commands.Sweep
open Common.Solvers
open Common.Utilities

    module private Solvers =
        let rand = new System.Random()
        let rec solveRandomly (solution:Solution) = 
            match solution.SolutionState with
            | Win | Dead -> solution
            | _ -> 
                let sweepableCells = getHiddenCells solution |> Array.ofSeq
                let cellToSweep = sweepableCells.[rand.Next(sweepableCells.Length)]
                let game = solution.Game |> sweep cellToSweep.Coords.X cellToSweep.Coords.Y
                let newSolution = getSolutionFromGame game
                solveRandomly newSolution


let randomSolver = solve Solvers.solveRandomly
