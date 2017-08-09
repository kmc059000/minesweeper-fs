module ProbabilitySolver

open Common

    module private Solvers =
        let rand = new System.Random()

        let getMineProbability solution exposedCell =
            let neighborMines = exposedCell.SurroundingCount |> float
            let flaggedCount = exposedCell |> Cell.getFlaggedCount solution
            let hiddenCount = exposedCell |> Cell.getHiddenCount solution
            (neighborMines - flaggedCount) / hiddenCount

        let solutionMineProbability solution =
            //# of remaining mines / number of hidden cells
            let totalMines = solution.Game.MineCount |> float
            let flaggedCount = solution |> Solution.flaggedCount |> float
            let hiddenCount = solution |> Solution.hiddenCount |> float
            (totalMines - flaggedCount) / hiddenCount

        //returns the probability of the hidden cell being a mine.
        //if there are any exposed neighbors, then the probability is the highest probability that this cell is that neighbors mine
        //otherwise, this cell's probability is the number of remaining mines / number of hidden cells
        let getCellProbability solution solutionProbability (cell:HiddenCell) =
            let probabilities = 
                Coordinate.getExposedNeighbors solution cell.Coords
                |> Seq.map (getMineProbability solution)
                |> Seq.toList
            let probability = 
                match probabilities.Length with
                | 0 -> (solutionProbability, solutionProbability)
                | _ -> (probabilities |> Seq.min, probabilities |> Seq.max)
            (cell, probability)

        let rec solveWithProbability (solution:Solution) = 
            match solution.SolutionState with
            | Win | Dead -> solution
            | _ -> 
                let solutionProbability = solutionMineProbability solution

                let cellsByProbability = 
                    solution
                    |> Solution.cellsToSeq 
                    |> Seq.choose Coordinate.getHiddenCell
                    |> Seq.map (getCellProbability solution solutionProbability)
                
                let cellsByMinProbability = cellsByProbability |> Seq.groupBy (fun (cell, (min, max)) -> min)

                let cellsToFlag =
                    cellsByProbability 
                    |> Seq.filter (fun (cell, (min, max)) -> max = 1.0)
                    |> Seq.map fst
                    |> Seq.toList
                
                let (probability, cellResults) = 
                    cellsByMinProbability
                    |> Seq.minBy fst

                let cellsToSweep = lazy ( cellResults |> Seq.map fst |> Seq.toList )

                let game, perfectSweeps, imperfectSweeps =
                    match (cellsToFlag, probability) with 
                    | [], 0.0 -> Game.sweepAll cellsToSweep.Value solution.Game, cellsToSweep.Value.Length, 0
                    | [], _ -> Game.sweepRandom cellsToSweep.Value rand solution.Game, 0, 1
                    | cells, _ -> Game.flagAll cells solution.Game, 0, 0
                    
                
                game 
                |> Solution.ofGame 
                |> Solution.withProbability (Some probability)
                |> Solution.withSweepCounts (solution.PerfectSweeps + perfectSweeps) (solution.ImperfectSweeps + imperfectSweeps)
                |> solveWithProbability
                    
                //find max probability of each sweepable cell of whether it is a mine or not
                //flag all that are 100% certain that it is a mine
                //sweep all that are 0% certain that it is a mine
                //if none, randomly choose 1 cell from those that have the highest probability
                //reevaulate cells


let probabilitySolver = Game.solve Solvers.solveWithProbability
