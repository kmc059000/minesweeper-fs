module Program

open Minesweeper
open Commands.Sweep
open Commands.Flag
open Commands.Move

let debug = false

let getCellChar (game:Game) (cell:Cell) =
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

let getRowText (game:Game) (idx:int) (row:Cell[]) = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat " " 
    let rowNum = (idx + 1).ToString().PadLeft(2,' ')
    sprintf "║%s║" rowText

let getHeader (game:Game) left right = 
    let inside =
        [0..(game.Width - 2)]
        |> Seq.map (fun x -> "══")
        |> String.concat ""
    left + inside + right 

let getGameDisplay (game:Game) =
    let rows = 
        game.Cells
        |> Array.chunkBySize game.Width
        |> Array.mapi (getRowText game)
        |> String.concat "\r\n"
    let headerTop = (getHeader game "╔═" "╗")
    let headerBottom = (getHeader game "╚═" "╝")
    let help = "Use arrow keys to move | Space to sweep | f to flag | q to quit"
    let stateMessage = 
        match game.State with
        | Start | Playing -> ""
        | Win -> 
            "You won!"
        | Dead -> 
            "You have exploded! :("
        | Quit -> 
            "Quitter!"
        | Exit -> ""

    sprintf "F# Minesweeper\r\n%s\r\n%s \r\n%s \r\n%s \r\n\r\n%s" help headerTop rows headerBottom stateMessage

let processMove (game:Game) key =
    let newState = 
        match key with
        | System.ConsoleKey.LeftArrow -> moveLeft game
        | System.ConsoleKey.RightArrow -> moveRight game
        | System.ConsoleKey.UpArrow -> moveUp game
        | System.ConsoleKey.DownArrow -> moveDown game
        | System.ConsoleKey.Q -> { game with State = GameState.Quit }
        | System.ConsoleKey.Spacebar -> sweep game game.CursorPosition.X game.CursorPosition.Y
        | System.ConsoleKey.F -> flag game game.CursorPosition.X game.CursorPosition.Y
        | _ -> game
    newState

//unpure method
let gameloop randomNumbers =
    let mutable game = createEasyGame randomNumbers
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
    let randomNumbers = [0..1000] |> List.map (fun _ -> rand.Next()) |> Array.ofList
    let game = gameloop randomNumbers
    match game.State with
    | Dead -> System.Console.ReadLine() |> ignore
    | _ -> ()
    0