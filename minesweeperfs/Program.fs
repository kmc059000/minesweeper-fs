module Program

open System

open Games
open Commands.Sweep
open Commands.Flag
open Commands.Move
open Commands.Quit

open ConsolePrinting
open MinesweeperUI

//debug <- true

let printGame previousDisplay game =
    let newDisplay = getGameDisplay game
    printConsoleText ConsoleCoords.origin previousDisplay newDisplay
    newDisplay

let getAction (cursor:Coordinates.Coordinate) key =
    let x = cursor.X
    let y = cursor.Y
    match key with
    | System.ConsoleKey.LeftArrow -> moveLeft
    | System.ConsoleKey.RightArrow -> moveRight
    | System.ConsoleKey.UpArrow -> moveUp
    | System.ConsoleKey.DownArrow -> moveDown
    | System.ConsoleKey.Q -> quit
    | System.ConsoleKey.Spacebar -> sweep x y
    | System.ConsoleKey.F -> flag x y
    | _ -> id

//unpure method
let gameloop randomNumbers =
    let mutable console = ConsoleText.emptyUI
    let mutable game = GameFactory.createMediumGame randomNumbers

    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        console <- printGame console game
        let key = System.Console.ReadKey().Key
        let handleAction = getAction game.CursorPosition key
        game <- handleAction game
    

[<EntryPoint>]
let main argv =
    let rand = new System.Random()
    gameloop (rand.Next())
    0