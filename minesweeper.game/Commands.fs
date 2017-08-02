namespace Commands

open Coordinates
open Cells
open Games

    module Common =        
        let setGameCellState index state game =
            let cell = game.Cells.[index]
            let newCells = game.Cells |> Map.add index { cell with State = state; }
            { game with Cells = newCells}

    module Sweep =
        let getSurroundingCellsToSweep index game =
            let cell = game.Cells.[index]
            match cell.State with
            | Hidden-> 
                Coordinates.getValidSurroundingIndexes cell.Coords
                    |> Seq.map (fun c -> c.Index)
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
                let newGame = game |> Common.setGameCellState x Exposed
                newGame |> sweepCells surrounding |> sweepCells xs


        let sweep game x y = 
            let index = Coordinates.getArrayIndex x y game.GameSize
            game 
                |> Game.tryPlaceMines index
                |> sweepCells [index]
                |> Game.testWin
                |> Game.testLoss index

    module Flag =
        let flag game x y = 
            let index = Coordinates.getArrayIndex x y game.GameSize
            game |> Common.setGameCellState index Flagged


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
