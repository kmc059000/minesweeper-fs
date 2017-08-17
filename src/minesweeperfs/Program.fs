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

type TopMenuResponse = PlayGame of (int -> Game) | Quit

type TopMenuChoice = { Text: string; Choice: TopMenuResponse; }
    with member this.Command = this.Text.ToLower().[0].ToString()

type TopMenuChoices = TopMenuChoices of TopMenuChoice list

    module TopMenuChoices =
        let tryFind key (TopMenuChoices choices) = 
            choices |> List.tryFind (fun c -> c.Command = key)

        let asStrings (TopMenuChoices choices) =
            choices |> Seq.map (fun c -> c.Command + " - " + c.Text)
            
    module TopMenuChoice =
        let create text resp = { Text= text; Choice = resp; }

let menuChoices = 
    TopMenuChoices [
        TopMenuChoice.create "Quit" Quit
        TopMenuChoice.create "Easy"  (PlayGame GameFactory.createEasyGame)
        TopMenuChoice.create "Medium"  (PlayGame GameFactory.createMediumGame)
        TopMenuChoice.create "Hard"  (PlayGame GameFactory.createMediumGame)
    ]

let resetColors _ = 
    Console.ForegroundColor <- ConsoleColor.Green
    Console.BackgroundColor <- ConsoleColor.Black

let rec requestGameSize _ = 
    resetColors()
    printfn "What difficulty would you like to play? (type the number)"
    menuChoices |> TopMenuChoices.asStrings |> Seq.iter (printfn "%s")

    let option = Console.ReadKey().KeyChar.ToString()
    let difficulty = TopMenuChoices.tryFind option menuChoices
    match difficulty with
    | Some resp -> resp.Choice
    | None ->
        Console.Clear()
        printfn "Unknown difficulty: %s" option
        requestGameSize()

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
    | ConsoleKey.A -> sweepAllHiddenNeighbors x y
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