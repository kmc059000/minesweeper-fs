module ProbabilitySolver

open Minesweeper
open Commands.Sweep
open Commands.Flag
open Common

    module private Solvers =
        let rand = new System.Random()

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

        let getNeighborsOfType typeMatcher solution coords =
            getValidSurroundingIndexes solution.Game.Width solution.Game.Height coords
            |> Seq.map (fun n -> solution.Cells.[n.Index])
            |> Seq.choose typeMatcher
                
        let getFlaggedNeighbors = getNeighborsOfType getFlaggedCell
        let getHiddenNeighbors = getNeighborsOfType getHiddenCell
        let getExposedNeighbors = getNeighborsOfType getExposedCell

        let getMineProbability solution exposedCell =
            let neighborMines = exposedCell.SurroundingCount |> float
            let flaggedCount = getFlaggedNeighbors solution exposedCell.Coords |> Seq.length |> float
            let hiddenCount = getHiddenNeighbors solution exposedCell.Coords |> Seq.length |> float
            let p = (neighborMines - flaggedCount) / hiddenCount
            p

        let solutionMineProbability solution =
            //# of remaining mines / number of hidden cells
            let totalMines = solution.Game.MineCount |> float
            let flaggedCount = solution.Cells |> Seq.choose getFlaggedCell |> Seq.length |> float
            let hiddenCount =  solution.Cells |> Seq.choose getHiddenCell |> Seq.length |> float
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
                | 0 -> solutionProbability
                | _ -> probabilities |> Seq.max
            (cell, probability)
            


        let rec sweepAll (cells:HiddenCell list) game =
            match cells with
            | [] -> game
            | x::xs ->
                sweep game x.Coords.X x.Coords.Y
                |> sweepAll xs
        
        let rec flagAll (cells:HiddenCell list) game =
            match cells with
            | [] -> game
            | x::xs ->
                flag game x.Coords.X x.Coords.Y
                |> flagAll xs

        let rec solveWithProbability (solution:Solution) = 
            match solution.SolutionState with
            | Win | Dead -> solution
            | _ -> 
                let solutionProbability = solutionMineProbability solution

                let cellsByProbability = 
                    getUnsolvedCells solution 
                    |> Seq.map (getCellProbability solution solutionProbability)
                    |> Seq.groupBy (fun (cell, prob) -> prob)
                
                let cellsToFlag = cellsByProbability |> Seq.tryFind (fun (p, _) -> p = 1.0) 
                
                let (probability, cellResults) = 
                    cellsByProbability
                    |> Seq.sortBy (fun (prob, cells) -> prob)
                    |> Seq.head

                let cells = cellResults |> Seq.map (fun (c,p) -> c) |> Seq.toList

                let game =
                    solution.Game
                    |> match (cellsToFlag, probability) with 
                        | Some (_, cells), _ -> flagAll (cells |> (Seq.map fst) |> Seq.toList)
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
