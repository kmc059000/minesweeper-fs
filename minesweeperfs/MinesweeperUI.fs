module MinesweeperUI

open System
open ConsolePrinting
open Cells
open Games

let mutable debug = false

let defaultText text = (text, ConsoleColor.Green)

let private getCellChar game cell =
    let mine = ("*", ConsoleColor.Red)
    let exposedChar = 
        match cell.IsMine with
            | true -> mine
            | false ->
                match cell.SurroundingCount with
                | None 
                | Some 0 -> defaultText " "
                | Some i -> 
                    let color = 
                        match i with
                        | 1 -> ConsoleColor.Cyan
                        | 2 -> ConsoleColor.DarkCyan
                        | 3 -> ConsoleColor.Yellow
                        | 4 -> ConsoleColor.DarkRed
                        | _ -> ConsoleColor.Red
                    (i.ToString(), color)
    match game.CursorPosition = cell.Coords with
    | true -> ("@", ConsoleColor.Green)
    | false ->
        match (cell.State, game.State) with
        | (_, Dead) -> exposedChar
        | (Hidden, _) -> 
            match debug with
            | true -> if cell.IsMine then mine else ("H", ConsoleColor.White)
            | false -> ("·", ConsoleColor.White)
        | (Exposed, _) ->  exposedChar
        | (Flagged, _) -> ("?", ConsoleColor.Magenta)

let private getRowText game row = 
    let inner = row |> List.map (getCellChar game) |> List.map (fun (text, color) -> (text + " ", color))
    [("║", ConsoleColor.Green)] @ inner @ [("║\r\n", ConsoleColor.Green)]

let private getRowsText game =
    game.Cells
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun c -> c.Coords.Index)
        |> List.chunkBySize game.GameSize.Width
        |> List.map (getRowText game)

let private getHeader game left right = 
    let inside =
        [0..(game.GameSize.Width - 2)]
        |> Seq.map (fun x -> "══")
        |> String.concat ""
    left + inside + right 


let getGameDisplay game =
    let help = "Use arrow keys to move | Space to sweep | f to flag | q to quit"
    let headerTop = getHeader game "╔═" "═╗"
    let headerBottom = getHeader game "╚═" "═╝"
    let rows = getRowsText game |> List.collect id
    let stateMessage = 
        match game.State with
        | Start | Playing | Exit -> ""
        | Win -> 
            "You won!"
        | Dead -> 
            "You have exploded! :("
        | Quit -> 
            "Quitter!"

    let top = [
        defaultText "F# Minesweeper\r\n";
        defaultText help;
        defaultText "\r\n";
        defaultText headerTop;
        defaultText "\r\n";
    ]
    let bottom = [
        defaultText headerBottom;
        defaultText "\r\n\r\n";
        defaultText stateMessage;
    ]
    (top @ rows @ bottom) |> ConsoleText.withCoords ConsoleCoords.origin