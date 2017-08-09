namespace Commands

open Coordinates
open Cells
open Games

    module Sweep =
        let getSurroundingCellsToSweep index game =
            let cell = game.Cells.[index]
            match cell.State with
            | Hidden-> 
                Coordinates.getValidSurroundingIndexes cell.Coords
                    |> Seq.map (Game.getCell game)
                    |> Seq.filter (fun x -> x.IsMine = false)
                    |> Seq.map (fun x -> x.Coords.Index)
                    |> List.ofSeq
            | _ -> []

        //this will auto-sweep the surrounding cells if the sweeped cell has 0 surrounding mines.
        let rec sweepCells indexes game =
            match indexes with 
            | [] -> game
            | x::xs ->
                let cell = game.Cells.[x]
                let surrounding =
                    match cell.SurroundingCount with
                    | Some 0 -> getSurroundingCellsToSweep x game
                    | _ -> []
                let newGame = game |> Game.setCellState x Exposed
                newGame |> sweepCells surrounding |> sweepCells xs


        let sweep x y game = 
            let index = Coordinates.getArrayIndex x y game.GameSize
            game 
            |> Game.tryPlaceMines index
            |> sweepCells [index]
            |> Game.testWin
            |> Game.testLoss index

    module Flag =
        let flag x y game = 
            let index = Coordinates.getArrayIndex x y game.GameSize
            let cell = Game.getCell game index
            let flag, flagDiff = 
                match cell.State with
                | CellState.Hidden -> Game.setCellState index Flagged, 1
                | CellState.Flagged -> Game.setCellState index Hidden, -1
                | _ -> id, 0
            let newGame = flag game
            { newGame with FlagCount = newGame.FlagCount + flagDiff }


    module Move =
        let move x y game =
            let newPosition = Coordinates.getOffsetIndex game.CursorPosition (x, y)
            match (Coordinates.isValid newPosition) with
            | true -> { game with CursorPosition = newPosition; }
            | false -> game
            
        let moveLeft = move -1 0
        let moveRight = move 1 0 
        let moveUp = move 0 -1
        let moveDown = move 0 1

    module Quit = 
        let quit game : Game = { game with State = GameState.Quit }
            
