module Program

open Cells
open Games
open Commands.Sweep
open Commands.Flag
open Commands.Move

let debug = false

let getCellChar game cell =
    let exposedChar = 
        match cell.IsMine with
            | true -> "*"
            | false ->
                match cell.SurroundingCount with
                | None 
                | Some 0 -> " "
                | Some i -> i.ToString()
    match game.CursorPosition = cell.Coords with
    | true -> "@"
    | false ->
        match (cell.State, game.State) with
        | (_, Dead) -> exposedChar
        | (Hidden, _) -> 
            match debug with
            | true -> if cell.IsMine then "*" else "H"
            | false -> "·"
        | (Exposed, _) ->  exposedChar
        | (Flagged, _) -> "?"

let getRowText game row = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat " "
    "║" + rowText + "║"

let getRowsText game =
    game.Cells
        |> Map.toArray
        |> Array.map snd
        |> Array.sortBy (fun c -> c.Coords.Index)
        |> Array.chunkBySize game.GameSize.Width
        |> Array.map (getRowText game)
        |> String.concat "\r\n"

let getHeader game left right = 
    let inside =
        [0..(game.GameSize.Width - 2)]
        |> Seq.map (fun x -> "══")
        |> String.concat ""
    left + inside + right 

let getGameDisplay game =
    let help = "Use arrow keys to move | Space to sweep | f to flag | q to quit"
    let headerTop = getHeader game "╔═" "╗"
    let headerBottom = getHeader game "╚═" "╝"
    let rows = getRowsText game
    let stateMessage = 
        match game.State with
        | Start | Playing | Exit -> ""
        | Win -> 
            "You won!"
        | Dead -> 
            "You have exploded! :("
        | Quit -> 
            "Quitter!"

    sprintf "F# Minesweeper\r\n%s\r\n%s \r\n%s \r\n%s \r\n\r\n%s" help headerTop rows headerBottom stateMessage

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
    let mutable game = GameFactory.createEasyGame randomNumbers
    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        //print UI
        System.Console.SetCursorPosition(0,0)
        printfn "%s" (getGameDisplay game)

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