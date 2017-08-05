module Program

open System

open Cells
open Games
open Commands.Sweep
open Commands.Flag
open Commands.Move

open ConsolePrinting

let debug = false

let defaultText text = (text, ConsoleColor.Green)

let getCellChar game cell =
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

let getRowText game row = 
    let inner = row |> List.map (getCellChar game) |> List.map (fun (text, color) -> (text + " ", color))
    [("║", ConsoleColor.Green)] @ inner @ [("║\r\n", ConsoleColor.Green)]

let getRowsText game =
    game.Cells
        |> Map.toList
        |> List.map snd
        |> List.sortBy (fun c -> c.Coords.Index)
        |> List.chunkBySize game.GameSize.Width
        |> List.map (getRowText game)

let getHeader game left right = 
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

let processMove game key = 
    match key with
    | System.ConsoleKey.LeftArrow -> moveLeft game
    | System.ConsoleKey.RightArrow -> moveRight game
    | System.ConsoleKey.UpArrow -> moveUp game
    | System.ConsoleKey.DownArrow -> moveDown game
    | System.ConsoleKey.Q -> { game with State = GameState.Quit }
    | System.ConsoleKey.Spacebar -> sweep game game.CursorPosition.X game.CursorPosition.Y
    | System.ConsoleKey.F -> flag game game.CursorPosition.X game.CursorPosition.Y
    | _ -> game


//unpure method
let gameloop randomNumbers =
    let mutable game = GameFactory.createMediumGame randomNumbers
    let mutable prevDisplay = List.empty
    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        //print UI
        let newDisplay = getGameDisplay game

        let lastPos = printConsoleText prevDisplay newDisplay ConsoleCoords.origin
        System.Console.SetCursorPosition(lastPos.X, lastPos.Y)


        prevDisplay <- newDisplay

        //get input from user
        let key = System.Console.ReadKey()

        //get new game state
        game <- processMove game key.Key
    game
    

[<EntryPoint>]
let main argv =
    let rand = new System.Random()
    let game = gameloop (rand.Next())
    0