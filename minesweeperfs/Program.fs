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

let rec requestGameSize _ = 
    System.Console.WriteLine("What difficulty would you like to play? (type the number)")
    System.Console.WriteLine("1 - Easy")
    System.Console.WriteLine("2 - Medium")
    System.Console.WriteLine("3 - Hard")
    System.Console.WriteLine("")
    let difficulty = System.Console.ReadLine()
    match difficulty with
    | "1" -> GameFactory.createEasyGame
    | "2" -> GameFactory.createMediumGame
    | "3" -> GameFactory.createHardGame
    | _ -> 
        System.Console.Clear()
        System.Console.WriteLine("Unknown difficulty: " + difficulty)
        requestGameSize ()



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
let gameloop seed =
    let gameFactory = requestGameSize ()
    System.Console.Clear()
    let mutable console = ConsoleText.emptyUI

    let mutable game = gameFactory seed

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