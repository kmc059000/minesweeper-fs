namespace Commands

open Coordinates
open Cells
open Games
open FSharpUtils

module Sweep =
    let private getNeighborsToSweep cell game =
        let cellState = Game.getCellStateFromCell cell game
        match cell.SurroundingCount, cellState with
        | Some 0, Hidden -> 
            game
            |> Game.getNeighborCells cell
            |> Seq.filterMap (fun c -> Game.isCellNotMine c.Coords game) Cells.getIndex
            |> List.ofSeq
        | _ -> []

    let trySetExposed (cell:Cell) game =
        let cellState = Game.getCellStateFromCell cell game
        match cellState with
        | CellState.Exposed -> game
        | _ -> 
            game 
            |> Game.setCellStateForCell cell Exposed
            |> Game.incrementExposedCount

    //this will auto-sweep the surrounding cells if the sweeped cell has 0 surrounding mines.
    let rec private sweepCells indexes game =
        match indexes with 
        | [] -> game
        | x::xs ->
            let cell = game.Cells.[x]
            let neighborsToSweep = getNeighborsToSweep cell game
            game
            |> trySetExposed cell           
            |> sweepCells (neighborsToSweep @ xs)


    let sweep x y game = 
        let index = Coordinates2.toIndex x y game.GameSize
        game 
        |> Game.tryPlaceMines index
        |> sweepCells [index]
        |> Game.testWin
        |> Game.testLoss index
    

    //sweeps all hidden cells that are neighbors of passed in position.
    let sweepAllHiddenNeighbors x y game =
        let rec loop cells game' =
            match cells with
            | [] -> game'
            | x::xs ->
                let (x',y',_) = Coordinates2.toXY x.Coords game.GameSize
                game'
                |> sweep x' y'
                |> loop xs

        let index = Coordinates2.toIndex x y game.GameSize
        let cell = Game.getCell game index
        let mineCount = cell.SurroundingCount |> Option.defaultValue 0

        let hiddenNeighbors = 
            game
            |> Game.filterNeighborCells cell (Game.isCellHidden game)
            |> List.ofSeq

        let flaggedCount = 
            game
            |> Game.filterNeighborCells cell (Game.isCellFlagged game)
            |> Seq.length

        let cellState = Game.getCellState index game
        match cellState, flaggedCount = mineCount with
        | Exposed, true -> loop hiddenNeighbors game
        | _ -> game
        
module Flag =
    let flag x y game = 
        let index = Coordinates2.toIndex x y game.GameSize
        let cellState = Game.getCellState index game
        let flag, flagDiff = 
            match cellState with
            | CellState.Hidden -> Game.setCellState index Flagged, 1
            | CellState.Flagged -> Game.setCellState index Hidden, -1
            | _ -> id, 0
        let newGame = flag game
        { newGame with FlagCount = newGame.FlagCount + flagDiff }
        |> Game.testWin


module Move =
    let move x y game =
        let newPosition = Coordinates2.getOffsetIndex game.GameSize game.CursorPosition (x, y)
        match (Coordinates2.isValid game.GameSize newPosition) with
        | true -> { game with CursorPosition = newPosition; }
        | false -> game
            
    let moveLeft = move -1 0
    let moveRight = move 1 0 
    let moveUp = move 0 -1
    let moveDown = move 0 1

module Quit = 
    let quit game : Game = { game with State = GameState.Quit }
            

