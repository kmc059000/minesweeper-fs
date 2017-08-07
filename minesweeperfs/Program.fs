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
    Console.WriteLine("What difficulty would you like to play? (type the number)")
    Console.WriteLine("1 - Easy")
    Console.WriteLine("2 - Medium")
    Console.WriteLine("3 - Hard")
    Console.WriteLine("")
    let difficulty = Console.ReadLine()
    match difficulty with
    | "1" -> GameFactory.createEasyGame
    | "2" -> GameFactory.createMediumGame
    | "3" -> GameFactory.createHardGame
    | _ -> 
        Console.Clear()
        Console.WriteLine("Unknown difficulty: " + difficulty)
        requestGameSize ()



let printGame previousDisplay game =
    let newDisplay = getGameDisplay game
    printConsoleText ConsoleCoords.origin previousDisplay newDisplay
    newDisplay

let getAction (cursor:Coordinates.Coordinate) key =
    let x = cursor.X
    let y = cursor.Y
    match key with
    | ConsoleKey.LeftArrow -> moveLeft
    | ConsoleKey.RightArrow -> moveRight
    | ConsoleKey.UpArrow -> moveUp
    | ConsoleKey.DownArrow -> moveDown
    | ConsoleKey.Q -> quit
    | ConsoleKey.Spacebar -> sweep x y
    | ConsoleKey.F -> flag x y
    | _ -> id

//unpure method
let gameloop seed =
    let gameFactory = requestGameSize ()
    Console.Clear()
    let mutable console = ConsoleText.emptyUI

    let mutable game = gameFactory seed

    while game.State <> GameState.Quit do
        console <- printGame console game
        let key = Console.ReadKey().Key
        let handleAction = getAction game.CursorPosition key
        game <- handleAction game
    

[<EntryPoint>]
let main argv =
    let rand = new Random()
    gameloop (rand.Next())
    0