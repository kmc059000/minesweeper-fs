﻿module Program

open System

open Games
open Commands.Sweep
open Commands.Flag
open Commands.Move
open Commands.Quit

open ConsolePrinting
open MinesweeperUI

//debug <- true

type TopMenuResponse = PlayGame of (int -> Game) | Quit

let resetColors _ = 
    Console.ForegroundColor <- ConsoleColor.Green
    Console.BackgroundColor <- ConsoleColor.Black

let rec requestGameSize _ = 
    resetColors()
    printfn "What difficulty would you like to play? (type the number)"
    printfn "q - Quit"
    printfn "1 - Easy"
    printfn "2 - Medium"
    printfn "3 - Hard"
    printfn ""

    let difficulty = Console.ReadKey().KeyChar
    match difficulty with
    | '1' -> PlayGame GameFactory.createEasyGame
    | '2' -> PlayGame GameFactory.createMediumGame
    | '3' -> PlayGame GameFactory.createHardGame
    | 'q' -> Quit
    | _ ->
        Console.Clear()
        printfn "Unknown difficulty: %s" (difficulty.ToString())
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



//unpure
let gameloop initialGame =
    resetColors()
    Console.Clear()
    let mutable console = ConsoleOutput.emptyUI
    let mutable game = initialGame

    while game.State <> GameState.Quit do
        console <- printGame console game
        let key = Console.ReadKey().Key
        let handleAction = getAction game.CursorPosition key
        game <- handleAction game

//unpure
let rec topGameLoop (rand:Random) =
    Console.Clear()

    let menuResponse = requestGameSize ()
    match menuResponse with
    | Quit -> ()
    | PlayGame createGame ->
        //play full game
        rand.Next() |> createGame |> gameloop

        //ask for another game
        topGameLoop rand
    

[<EntryPoint>]
let main argv =
    let rand = new Random()
    topGameLoop rand
    0