open Minesweeper

let getCellChar (game:Game) (cell:Cell) =
    match cell.State with
    | Hidden -> "·"
    | Exposed -> 
        match getSurroundingCount game cell with
        | 0 -> " "
        | i -> i.ToString()
    | Flagged -> "?"

let getRowText (game:Game) (row:Cell[]) = 
    row |> Array.map (getCellChar game) |> String.concat ""

let printGame (game:Game) =
    let rows = 
        game.Cells
        |> Array.chunkBySize game.Width
        |> Array.map (getRowText game)
    printfn "Game:"
    for r in rows do printfn "%s" r
    printfn ""
    printfn ""

let processMove (game:Game) =
    printGame game
    
    match game.State with
    | Start -> printfn "Choose your first move:"
    | Playing -> printfn "Choose your next move:"
    | Win -> printfn "You won!"
    | Dead -> printfn "You have exploded! :("
    | Quit -> printfn "Quitter!"

    let moveArgs = System.Console.ReadLine().Split("".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
    let newState = 
        match moveArgs |> Array.tryHead with
        | None -> 
            printfn "Unknown command"
            game
        | Some c -> 
            match c with 
            | "sweep" -> game
            | "flag" -> game
            | "quit" -> {game with State = GameState.Quit}
            | "help" -> 
                printfn "Available commands:"
                printfn "sweep x y"
                printfn "flag x y"
                printfn "quit"
                game
            | _ -> 
                printfn "Unknown command"
                game
    printfn ""
    printfn ""
    newState

let gameloop (rand:System.Random) =
    let mutable game = createEasyGame (new System.Random())
    while game.State <> GameState.Dead && game.State <> GameState.Quit do
        game <- processMove game
    
    

[<EntryPoint>]
let main argv = 
    gameloop (new System.Random())
    0