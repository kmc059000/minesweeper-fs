module BetterProbabilitySolver

open Common
open Coordinates.Coordinates
open Game
open Cells
open System
open Utilities

type private Range<'a> = { Min: 'a; Max : 'a }
type private CellProbability = { Cell: HiddenCell; ProbabilityRange: Range<float>; }

type PatternType = Flaggable | Sweepable

type PatternCellValue = char
type OffsetPatternCell = { Pattern: PatternCellValue; RowOffset: int; ColOffset: int; Type: PatternType; }
type Pattern = { Offsets: OffsetPatternCell[,]; Type: PatternType }

type OffsetCoordinate = { Pattern: PatternCellValue; Coordinates: Coordinates.Coordinate; }
type OffsetCell = { Pattern: PatternCellValue; Cell: Cell option; }


module OffsetCoordinates =
    let isValid offsetCoords = Coordinates.Coordinates.isValid offsetCoords.Coordinates

    let toOffsetCell solution offsetCoordinate =
        let valid = isValid offsetCoordinate
        let cell = 
            if valid 
            then Some (Games.Game.getCell solution.Game offsetCoordinate.Coordinates.Index) 
            else None
        { Pattern = offsetCoordinate.Pattern; Cell = cell }

module OffsetPatternCell =    
    let getOffsetCoordinates coords (offsets:OffsetPatternCell[,]) =
        let getOffsetCoordinate x y =
            let patternCell = offsets.[x,y]
            let offsetCoords = Coordinates.Coordinates.getOffsetIndex coords (patternCell.RowOffset, patternCell.ColOffset)
            { Pattern = patternCell.Pattern; Coordinates = offsetCoords; }

        Array2D.initFrom offsets getOffsetCoordinate

module OffsetCell =
    let isMatch offsetCell =
        let cell = offsetCell.Cell
        let cellState = cell |> Option.map Cells.getState
        match offsetCell.Pattern, cellState with
        | '*',Some CellState.Hidden -> true
        | '*',Some CellState.Flagged -> true

        | 'E',None -> true

        | 'H',Some CellState.Hidden 
        | '$',Some CellState.Hidden 
        | 'H',Some CellState.Exposed 
        | '$',Some CellState.Exposed -> true

        | 'H',Some _ -> false
        | '$',Some _ -> false

        | '_',Some CellState.Exposed -> true
        
        | 'F',Some CellState.Flagged -> true
        | 'F',Some _ -> false

        | _,Some CellState.Exposed -> 
            match cell.Value.SurroundingCount with
            | Some c -> c.ToString() = offsetCell.Pattern.ToString()
            | None -> false
        | _,_ -> false

module PatternMatching =
    let patternMatchesCells solution offsets = 
        let toOffsetCell = OffsetCoordinates.toOffsetCell solution
        offsets
        |> Array2D.toSeq
        |> Seq.map toOffsetCell
        |> Seq.all OffsetCell.isMatch

    let isPatternMatch solution (cell:HiddenCell) pattern =
        let result = 
            pattern.Offsets
            |> OffsetPatternCell.getOffsetCoordinates cell.Coords
            |> patternMatchesCells solution
        
        if result then (Some pattern.Type) else None

module Patterns =
    module PatternFactory =
        let createPatternFromAnchors (pattern:_[,]) =
            let anchorPositions =
                let length1, length2 = Array2D.lengths pattern
                seq { 
                    for row in 0..(length1 - 1) do 
                        for col in 0..(length2 - 1) do 
                            if pattern.[row,col] = '*'
                            then yield (row, col, Flaggable)
                            else if pattern.[row,col] = '$'
                            then yield (row,col, Sweepable)
                }

            let getOffsetCell (asteriskRow, asteriskCol, patternType) = 
                let getCell row col =
                    { 
                        Pattern = pattern.[row, col]; 
                        RowOffset = row - asteriskRow; 
                        ColOffset = col - asteriskCol; 
                        Type = patternType 
                    }

                let offsets = Array2D.initFrom pattern getCell
                {
                    Offsets = offsets;
                    Type = patternType;
                }

            anchorPositions
            |> Seq.map getOffsetCell
    
        let createPatterns strs =
            let createPatternRotations (pattern:string) =
                pattern.Split("|".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
                |> array2D
                |> Array2D.allRotations

            strs 
            |> Seq.collect createPatternRotations
            |> Seq.collect createPatternFromAnchors
            |> Seq.toList

    //* = mine, $ = empty cell, _ is an exposed cell with any count, E = edge, F = flagged, H = hidden
    let patterns = 
        [ 
            "___|" +
            "121|" +
            "*$*|";

            //the 1 could have other mines not accounted for in the pattern
            //"__F|" +
            //"13F|" +
            //"HH$";

            "___E|" +
            "F21E|" +
            "$HHE";

            "F___|" +
            "F32_|" +
            "_HH*";

            "__F|" +
            "13_|" +
            "HH*";

            "___|" +
            "13F|" +
            "HH*";

            "E_FF|" +
            "E23$|" +
            "EHH$|";
            
            "E_F_|" +
            "E23F|" +
            "EHH$|";

            "E___|" +
            "E12_|" +
            "EHH*|";

            "E___|" +
            "E111|" +
            "EHH$|";

            "$$$|" +
            "H1_|" +
            "H1_|" +
            "EEE|";

            "*_F|" +
            "H3_|" +
            "H2F|" +
            "EEE|";

            "F_*|" +
            "_3H|" +
            "_1H|" +
            "EEE|";

            "_F$E|" +
            "F3HE|" +
            "_2HE|" +
            "EEEE|";

            "____|" + 
            "1221|" +
            "$**$|";
        ]
        |> PatternFactory.createPatterns

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
    let getMineProbabilityOfCell solution (exposedCell:ExposedCell) =
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
    

    let getPatternMatch (cell:HiddenCell) solution =
        let testPattern = PatternMatching.isPatternMatch solution cell
        Patterns.patterns 
        |> Seq.choose testPattern
        |> Seq.distinct
        //prioritize empty cells over flaggable cells
        |> Seq.sortBy (fun t -> if t = Sweepable then 0 else 1)
        |> Seq.tryHead
        

    //returns the probability of the hidden cell being a mine.
    //if there are any exposed neighbors, then the probability is the highest probability that this cell is that neighbors mine
    //otherwise, this cell's probability is the number of remaining mines / number of hidden cells
    let getCellProbability solution solutionProbability (cell:HiddenCell) =
        let probabilities = 
            cell.Coords 
            |> Coordinate.getExposedNeighbors solution 
            |> Seq.map (getMineProbabilityOfCell solution)
            |> Seq.toList
        let probabilityRange = 
            match probabilities.Length with
            | 0 -> { Min = solutionProbability; Max = solutionProbability }
            | _ -> { Min = probabilities |> Seq.min; Max = probabilities |> Seq.max }

        let defaultResult = { Cell = cell; ProbabilityRange = probabilityRange; }

        match probabilityRange.Min, probabilityRange.Max with
        | min, max when min <> 0.0 && max <> 1.0 ->
            let matchedPattern = getPatternMatch cell solution
            match matchedPattern with
            | Some Sweepable ->
                //printfn "matched empty cell pattern"
                { Cell = cell; ProbabilityRange = { Min = 0.0; Max = 0.0 }; }
            | Some Flaggable -> 
                //printfn "matched mine pattern"
                { Cell = cell; ProbabilityRange = { Min = 1.0; Max = 1.0 }; }
            | None -> defaultResult
        | _ -> defaultResult    

    let getCellProbabilities solution =  
        let solutionProbability = solutionMineProbability solution
        solution
        |> Solution.cellsToSeq 
        |> Seq.choose Coordinate.getHiddenCell
        |> Seq.map (getCellProbability solution solutionProbability)

let rand = new System.Random()

let solveStep solution =
    let cellProbabilities = ProbabilityCalculator.getCellProbabilities solution |> Seq.toList
    let cellsToFlag = CellProbability.cellsToFlag cellProbabilities
    let bestProbability, bestCellsToSweep = CellProbability.bestCellsToSweep cellProbabilities
                    
    let command, perfectSweeps, imperfectSweeps, bestProbability =
        let toCoords (cells:HiddenCell list) = cells |> Seq.map (fun c -> c.Coords) |> Set.ofSeq
        let bestCoordsToSweep = bestCellsToSweep |> toCoords
        match (cellsToFlag, bestProbability) with
        | [], 0.0 -> Sweep bestCoordsToSweep, bestCellsToSweep.Length, 0, 0.0
        | [], _ -> SweepRandom bestCoordsToSweep, 0, 1, bestProbability
        | mines, _ -> Flag (mines |> toCoords), 0, 0, 0.0

    command, perfectSweeps, imperfectSweeps, bestProbability
    
let nextCommand solution = 
    match solution.SolutionState with
        | Win | Dead -> None
        | _ ->
            let command, _, _, _ = solveStep solution
            Some command

let handleCommand command perfectSweeps imperfectSweeps bestProbability solution =
    let game =
        match command with
        | Sweep coords -> Game.sweepAllCoords (coords |> Set.toList) solution.Game
        | SweepRandom coords -> Game.sweepRandomCoord coords rand solution.Game
        | Flag coords -> Game.flagAllCoords (coords |> Set.toList) solution.Game

    game 
    |> Solution.ofGame 
    |> Solution.withProbability bestProbability
    |> Solution.withSweepCounts solution.Game.State perfectSweeps imperfectSweeps

let rec solve solution = 
    match solution.SolutionState with
    | Win | Dead -> solution
    | _ ->
        let command, perfectSweeps, imperfectSweeps, bestProbability = solveStep solution

        solution 
        |> (handleCommand command perfectSweeps imperfectSweeps bestProbability)
        |> solve


