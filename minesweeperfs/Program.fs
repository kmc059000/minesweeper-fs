open Minesweeper

let getCellChar (game:Game) (cell:Cell) =
    match cell.State with
    | Hidden -> "·"
    | Exposed -> 
        match getSurroundingCount game cell with
        | 0 -> " "
        | i -> i.ToString()
    | Flagged -> "?"

let getRowText (game:Game) (idx:int) (row:Cell[]) = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat "" 
    sprintf "%02i %s" (idx + 1) rowText

let inc x = x + 1
let getHeader (game:Game) (leftPadding:string) = 
    let width = game.Width
    let maxChars = width.ToString().Length
    let indexes = 
        [0..(width - 1)]
        |> Seq.map inc
        |> Seq.map (fun x -> x.ToString().PadLeft(maxChars, '0'))
        |> Array.ofSeq
    [0..(maxChars - 1)]
        |> List.map (fun i -> 
            indexes 
            |> Array.map (fun x -> x.[i].ToString())
            |> String.concat "")
        |> List.map (fun s -> leftPadding + s)
        |> String.concat "\r\n"


let printGame (game:Game) =
    let rows = 
        game.Cells
        |> Array.chunkBySize game.Width
        |> Array.mapi (getRowText game)
    printfn "Game:"
    printfn "%s" (getHeader game "   ")
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
    | Exit -> ()

    if game.State = Dead
    then
        let wait = System.Console.ReadLine()
        { game with State = Exit }
    else
        let moveArgs = System.Console.ReadLine().Split("".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        let newState = 
            match moveArgs |> List.tryHead with
            | None -> 
                printfn "Unknown command"
                game
            | Some c -> 
                match moveArgs with 
                | "sweep"::x::y::_ -> sweep game ((int x) - 1) ((int y) - 1)
                | "flag"::x::y::_ -> game
                | "quit"::_ -> { game with State = GameState.Quit }
                | "help"::_ -> 
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
    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        game <- processMove game
    
    

[<EntryPoint>]
let main argv = 
    gameloop (new System.Random())
    0