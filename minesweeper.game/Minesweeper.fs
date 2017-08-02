module Minesweeper

open Coordinates
open Cells
open Cells.Cells
open Games




let tryPlaceMine mineLocations cell =
    { cell with IsMine = Set.contains cell.Coords.Index mineLocations }


let placeMines game lastSelectedIndex = 
    let maxIndex = game.GameSize.Width * game.GameSize.Height
    
    let mineLocations =
        Seq.initInfinite  (fun i -> game.Random.Next(maxIndex))
        |> Seq.distinct
        //omit the first cell
        |> Seq.filter (fun c -> c <> lastSelectedIndex)
        |> Seq.take game.MineCount
        |> Set.ofSeq

    let newCells = 
        game.Cells
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (tryPlaceMine mineLocations)
        |> Seq.map (Cells.withSurroundingCount mineLocations)
        |> Seq.map (fun c -> (c.Coords.Index,c))
        |> Map.ofSeq
       
    { game with Cells = newCells; MineLocations = Some mineLocations; State = Playing }

let tryPlaceMines lastSelectedIndex (game:Game) =
    match game.State with
    | GameState.Start -> placeMines game lastSelectedIndex
    | _ -> game

let isWin (cells:Map<int, Cell>) = 
    cells
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
    |> not

let testWin (game:Game) =
    let isWin = isWin game.Cells
    match isWin with
    | true -> { game with State = Win }
    | false -> game

let testLoss index (game:Game) = 
    let cell = game.Cells.[index]
    match cell.IsMine with
    | true -> { game with State = GameState.Dead; }
    | false -> game