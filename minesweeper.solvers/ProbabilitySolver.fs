module ProbabilitySolver

open Minesweeper
open Common.Solvers
open Common.Utilities

    module private Solvers =
        let rand = new System.Random()

        let getMineProbability solution exposedCell =
            let neighborMines = exposedCell.SurroundingCount |> float
            let flaggedCount = getFlaggedNeighbors solution exposedCell.Coords |> Seq.length |> float
            let hiddenCount = getHiddenNeighbors solution exposedCell.Coords |> Seq.length |> float
            let p = (neighborMines - flaggedCount) / hiddenCount
            p

        let solutionMineProbability solution =
            //# of remaining mines / number of hidden cells
            let totalMines = solution.Game.MineCount |> float
            let cells = solutionCellsOfSeq solution
            let flaggedCount = cells |> Seq.choose getFlaggedCell |> Seq.length |> float
            let hiddenCount =  cells |> Seq.choose getHiddenCell |> Seq.length |> float
            (totalMines - flaggedCount) / hiddenCount

        //returns the probability of the hidden cell being a mine.
        //if there are any exposed neighbors, then the probability is the highest probability that this cell is that neighbors mine
        //otherwise, this cell's probability is the number of remaining mines / number of hidden cells
        let getCellProbability solution solutionProbability (cell:HiddenCell) =
            let probabilities = 
                getExposedNeighbors solution cell.Coords
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
                    solutionCellsOfSeq solution
                    |> Seq.choose getHiddenCell
                    |> Seq.map (getCellProbability solution solutionProbability)
                    //|> Seq.groupBy (fun (cell, prob) -> prob)
                
                let cellsByMaxProbability = cellsByProbability |> Seq.groupBy (fun (cell, (min, max)) -> max)
                let cellsByMinProbability = cellsByProbability |> Seq.groupBy (fun (cell, (min, max)) -> min)

                let cellsToFlag = 
                    cellsByMaxProbability 
                    |> Seq.tryFind (fun (p, _) -> p = 1.0) 
                    |> Option.map (fun (_, cells) -> Seq.map fst cells |> Seq.toList)
                
                let (probability, cellResults) = 
                    cellsByMinProbability
                    |> Seq.sortBy fst
                    |> Seq.head

                let cells = cellResults |> Seq.map fst |> Seq.toList

                let game =
                    solution.Game
                    |> match (cellsToFlag, probability) with 
                        | Some cells, _ -> flagAll cells
                        | None, probability -> 
                            match probability with
                            | 0.0 -> sweepAll cells
                            | _ -> 
                                let idx = rand.Next(cells.Length)
                                sweepAll [List.item idx cells]

                let newSolution = getSolutionFromGame game
                solveWithProbability newSolution
                    
                //find max probability of each sweepable cell of whether it is a mine or not
                //flag all that are 100% certain that it is a mine
                //sweep all that are 0% certain that it is a mine
                //if none, randomly choose 1 cell from those that have the highest probability
                //reevaulate cells


let probabilitySolver = solve Solvers.solveWithProbability
