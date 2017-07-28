module Program

open Minesweeper
open Commands.Sweep
open Commands.Flag

let debug = false

let getCellChar (game:Game) (cell:Cell) =
    match cell.State with
    | Hidden -> 
        match debug with
        | true -> if cell.IsMine then "* " else "H "
        | false -> "· "
    | Exposed -> 
        match cell.SurroundingCount with
        | None 
        | Some 0 -> "  "
        | Some i -> i.ToString() + " "
    | Flagged -> "? "

let getRowText (game:Game) (idx:int) (row:Cell[]) = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat "" 
    let rowNum = (idx + 1).ToString().PadLeft(2,' ')
    sprintf "%s║%s║%s" rowNum (rowText.Trim()) rowNum

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
        | Start -> "Choose your first move:"
        | Playing -> "Choose your next move:"
        | Win -> "You won!"
        | Dead -> "You have exploded! :("
        | Quit -> "Quitter!"
        | Exit -> ""

    sprintf "Game: \r\n%s \r\n%s \r\n%s \r\n\r\n%s" header rows header stateMessage

    

let processMove (game:Game) =
    printfn "%s" (getGameDisplay game)

    if game.State = Dead
    then
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
                | ["flag"; x; y] -> flag game ((int x) - 1) ((int y) - 1)
                | ["sweep"; x; y] -> sweep game ((int x) - 1) ((int y) - 1)
                | [x; y] -> sweep game ((int x) - 1) ((int y) - 1)
                | ["q"] | ["quit"] -> { game with State = GameState.Quit }
                | ["help"] -> 
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