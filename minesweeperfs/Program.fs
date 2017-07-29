module Program

open Minesweeper
open Commands.Sweep
open Commands.Flag
open Commands.Move

let debug = false

let getCellChar (game:Game) (cell:Cell) =
    match game.CursorPosition = cell.Coords with
    | true -> "@"
    | false -> 
        match cell.State with
        | Hidden -> 
            match debug with
            | true -> if cell.IsMine then "*" else "H"
            | false -> "·"
        | Exposed -> 
            match cell.SurroundingCount with
            | None 
            | Some 0 -> " "
            | Some i -> i.ToString()
        | Flagged -> "?"

let getRowText (game:Game) (idx:int) (row:Cell[]) = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat " " 
    let rowNum = (idx + 1).ToString().PadLeft(2,' ')
    sprintf "%s║%s║%s" rowNum (rowText) rowNum

let inc x = x + 1
let getHeader (game:Game) (leftPadding:string) = 
    let width = game.Width
    let maxChars = width.ToString().Length
    let indexes = 
        [0..(width - 1)]
        |> Seq.map inc
        |> Seq.map (fun x -> "═" + x.ToString().PadLeft(maxChars, ' ') + "═")
        |> Array.ofSeq
    [0..(maxChars + 1)]
        |> List.map (fun i -> 
            indexes 
            |> Array.map (fun x -> x.[i].ToString())
            |> String.concat (if i = 0 || i = maxChars + 1 then "═" else " "))
        |> List.map (fun s -> leftPadding + "║" + s + "║")
        |> String.concat "\r\n"


let getGameDisplay (game:Game) =
    let rows = 
        game.Cells
        |> Array.chunkBySize game.Width
        |> Array.mapi (getRowText game)
        |> String.concat "\r\n"
    let header = (getHeader game "  ")
    let stateMessage = 
        match game.State with
        | Start | Playing -> 
            "Use arrow keys to move.\r\nSpace to sweep\r\nf to flag.\r\nq to quit"
        | Win -> 
            "                       \r\n              \r\n          \r\nYou won! "
        | Dead -> 
            "                       \r\n              \r\n          \r\nYou have exploded! :("
        | Quit -> 
            "                       \r\n              \r\n          \r\nQuitter! "
        | Exit -> ""

    sprintf "Game: \r\n%s \r\n%s \r\n%s \r\n\r\n%s" header rows header stateMessage

    

let processMove (game:Game) =
    System.Console.SetCursorPosition(0,0)
    printfn "%s" (getGameDisplay game)

    if game.State = Dead
    then
        { game with State = Exit }
    else
        let key = System.Console.ReadKey()
        let newState = 
            match key.Key with
            | System.ConsoleKey.LeftArrow -> moveLeft game
            | System.ConsoleKey.RightArrow -> moveRight game
            | System.ConsoleKey.UpArrow -> moveUp game
            | System.ConsoleKey.DownArrow -> moveDown game
            | System.ConsoleKey.Q -> { game with State = GameState.Quit }
            | System.ConsoleKey.Spacebar -> sweep game game.CursorPosition.X game.CursorPosition.Y
            | System.ConsoleKey.F -> flag game game.CursorPosition.X game.CursorPosition.Y
            | _ -> game
        newState

let gameloop randomNumbers =
    let mutable game = createEasyGame randomNumbers
    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        game <- processMove game
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