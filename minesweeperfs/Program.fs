module Program

open System

open Cells
open Games
open Commands.Sweep
open Commands.Flag
open Commands.Move

open ConsolePrinting
open MinesweeperUI

//debug <- true


let printGame previousDisplay game =
    let printGame = printConsoleText ConsoleCoords.origin
    let newDisplay = getGameDisplay game
    printGame previousDisplay newDisplay
    newDisplay

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
    let mutable console = ConsoleText.emptyUI
    let mutable game = GameFactory.createMediumGame randomNumbers

    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        console <- printGame console game
        game <- System.Console.ReadKey().Key |> processMove game
    

[<EntryPoint>]
let main argv =
    let rand = new System.Random()
    gameloop (rand.Next())
    0