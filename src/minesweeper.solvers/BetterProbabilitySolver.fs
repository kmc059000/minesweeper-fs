﻿module BetterProbabilitySolver

open Common

type private Range<'a> = { Min: 'a; Max : 'a }
type private CellProbability = { Cell: HiddenCell; ProbabilityRange: Range<float>; }

module private CellProbability =
    let cellsToFlag cells =
        cells
        |> Seq.filter (fun x -> x.ProbabilityRange.Max = 1.0)
        |> Seq.map (fun x -> x.Cell)
        |> Seq.toList

    let bestCellsToSweep cells =
        let probability, probabilitiesToSweep = 
            cells 
            |> Seq.groupBy (fun x -> x.ProbabilityRange.Min)
            |> Seq.minBy (fun (prob,_) -> prob)

        let cellsToSweep = probabilitiesToSweep |> Seq.map (fun x -> x.Cell) |> Seq.toList
        probability, cellsToSweep

module private ProbabilityCalculator =
    let getMineProbabilityOfCell solution exposedCell =
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
    

    let getPatternProbability (cell:HiddenCell) solution =
        let neighbors = 
            cell.Coords 
            |> Coordinate.getNeighbors solution
            |> Seq.map (fun c -> 
                match c with
                | Hidden h -> "H"
                | Exposed e -> e.SurroundingCount.ToString()
                | Flagged f -> "F")
            |> Seq.toList

        match neighbors with
        | ["1"; "2"; "1"; "H"; "H"; "H"; _; _; _;] 
        | [_; _; _; "H"; "H"; "H"; "1"; "2"; "1";] 
        | [_; "H"; "1"; _; "H"; "2"; _; "H"; "1";]
        | ["1"; "H"; _; "2"; "H"; _; "1"; "H"; _;] -> Some 0.0
        | _ -> None

    //returns the probability of the hidden cell being a mine.
    //if there are any exposed neighbors, then the probability is the highest probability that this cell is that neighbors mine
    //otherwise, this cell's probability is the number of remaining mines / number of hidden cells
    let getCellProbability solution solutionProbability (cell:HiddenCell) =
        let patternProbability = getPatternProbability cell solution

        match patternProbability with
        | Some p -> { Cell = cell; ProbabilityRange = { Min = p; Max = p }; }
        | None -> 
            let probabilities = 
                cell.Coords 
                |> Coordinate.getExposedNeighbors solution 
                |> Seq.map (getMineProbabilityOfCell solution)
                |> Seq.toList
            let probabilityRange = 
                match probabilities.Length with
                | 0 -> { Min = solutionProbability; Max = solutionProbability }
                | _ -> { Min = probabilities |> Seq.min; Max = probabilities |> Seq.max }
            { Cell = cell; ProbabilityRange = probabilityRange; }

    let getCellProbabilities solution =  
        let solutionProbability = solutionMineProbability solution
        solution
        |> Solution.cellsToSeq 
        |> Seq.choose Coordinate.getHiddenCell
        |> Seq.map (getCellProbability solution solutionProbability)

let rand = new System.Random(1)

let rec solve solution = 
    match solution.SolutionState with
    | Win | Dead -> solution
    | _ ->
        let cellProbabilities = ProbabilityCalculator.getCellProbabilities solution
        let cellsToFlag = CellProbability.cellsToFlag cellProbabilities
        let bestProbability, bestCellsToSweep = CellProbability.bestCellsToSweep cellProbabilities
                
        let game, perfectSweeps, imperfectSweeps =
            match (cellsToFlag, bestProbability) with 
            | [], 0.0 -> Game.sweepAll bestCellsToSweep solution.Game, bestCellsToSweep.Length, 0
            | [], _ -> Game.sweepRandom bestCellsToSweep rand solution.Game, 0, 1
            | mines, _ -> Game.flagAll mines solution.Game, 0, 0
                    
        game 
        |> Solution.ofGame 
        |> Solution.withProbability bestProbability
        |> Solution.withSweepCounts solution.Game.State perfectSweeps imperfectSweeps
        |> solve


