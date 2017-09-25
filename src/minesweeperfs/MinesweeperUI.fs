﻿module MinesweeperUI

open System
open ConsolePrinting
open Coordinates
open Cells
open Games

let mutable debug = false

let private defaultText text = ConsoleString.create text ConsoleColor.Green ConsoleColor.Black

let private emptyText = defaultText " "
let private mineText =  ConsoleString.create "*" ConsoleColor.White ConsoleColor.DarkRed
let private cursorText = ConsoleString.create "#" ConsoleColor.Black ConsoleColor.Green
let private flagText =  ConsoleString.create "?" ConsoleColor.Black ConsoleColor.Yellow
let private hiddenCellText = ConsoleString.create "·" ConsoleColor.White ConsoleColor.Black
let private hiddenCellDebugText = ConsoleString.create "H" ConsoleColor.White ConsoleColor.Black

let private highlightCursorNeighbor game cell str =
    let isNeighbor = Coordinates.isNeighbor game.CursorPosition cell.Coords
    match isNeighbor with
    | true -> { str with Background = ConsoleColor.DarkGray }
    | _ -> str

let private withCommand game cell command str =
    let coords = cell.Coords
    match command with
    | None -> str
    | Some (Common.Flag commandCoords) -> 
        match (Set.contains coords commandCoords) with
        | true -> { str with Text = "F"; Background = ConsoleColor.Red; }
        | false -> str
    | Some (Common.Sweep commandCoords) -> 
        match (Set.contains coords commandCoords) with
        | true -> { str with Text = "S"; Background = ConsoleColor.Green; }
        | false -> str
    | Some (Common.SweepRandom (commandCoords, _)) -> 
        match (Set.contains coords commandCoords) with
        | true -> { str with Text = "R"; Background = ConsoleColor.DarkGreen; }
        | false -> str

let private hiddenCell game cell =
    match debug, cell.IsMine with
    | true,true -> mineText
    | true, false -> hiddenCellDebugText
    | false, _ -> hiddenCellText

let private getExposedCharText game cell =
    match cell.IsMine, cell.SurroundingCount with
    | true, _ -> mineText
    | false, None
    | false, Some 0 -> emptyText
    | false, Some i -> 
        let color = 
            match i with
            | 1 -> ConsoleColor.Cyan
            | 2 -> ConsoleColor.DarkCyan
            | 3 -> ConsoleColor.Yellow
            | 4 -> ConsoleColor.DarkRed
            | _ -> ConsoleColor.Red
        ConsoleString.create (i.ToString()) color ConsoleColor.Black           
            
let private getCellChar game command cell =
    let exposedChar = lazy (getExposedCharText game cell)
    match game.CursorPosition = cell.Coords with
    | true -> cursorText
    | false ->
        match (cell.State, game.State) with
        | (_, Dead) 
        | (Exposed, _) ->  exposedChar.Value
        | (Hidden, _) -> 
            hiddenCell game cell 
            |> highlightCursorNeighbor game cell
            |> withCommand game cell command
        | (Flagged, _) -> flagText        

let private getRowText game command row =
    let left = ConsoleString.create "║" ConsoleColor.Green ConsoleColor.Black
    let right = ConsoleString.create "║\r\n" ConsoleColor.Green ConsoleColor.Black
    let inner = 
        row 
        |> Seq.map (getCellChar game command)
        |> Seq.collect (fun cellChar -> [cellChar; (defaultText " ")])
        |> Seq.toList
    left :: (inner @ [right])

let private getRowsText command game =
    game.Cells
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun c -> c.Coords.Index)
        |> List.chunkBySize game.GameSize.Width
        |> List.map (getRowText game command)

let private getGameBorder left right game = 
    let inside =
        [0..(game.GameSize.Width - 2)]
        |> Seq.map (fun x -> "══")
        |> String.concat ""
    left + inside + right 

let private getGameTop = getGameBorder "╔═" "═╗"
let private getGameBottom = getGameBorder "╚═" "═╝"

let private getRemainingMineCount game =
    sprintf "Remaining Mines: %03i" (game.MineCount - game.FlagCount)

let private getGameMessage game =
    match game.State with
    | Start | Playing -> ""
    | Win -> 
        "You won!"
    | Dead -> 
        "You have exploded! :("
    | Quit -> 
        "Quitter!"

let getGameDisplay game (command:Common.Command option) = 
    let rows = game |> getRowsText command |> List.collect id        

    let top = [
        defaultText "F# Minesweeper\r\n";
        defaultText "Use arrow keys to move | Space to sweep | f to flag | q to quit";
        defaultText "\r\n";
        defaultText (getRemainingMineCount game);
        defaultText "\r\n";
        defaultText (getGameTop game);
        defaultText "\r\n";
    ]
    let bottom = [
        defaultText (getGameBottom game);
        defaultText "\r\n\r\n";
        defaultText (getGameMessage game);
    ]
    (top @ rows @ bottom) |> ConsoleOutput.withCoords ConsoleCoords.origin