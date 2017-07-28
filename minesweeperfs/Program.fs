module Program

open Minesweeper
open Commands

let debug = false

let getCellChar (game:Game) (cell:Cell) =
    match cell.State with
    | Hidden -> 
        match debug with
        | true -> if cell.IsMine then "*" else "H"
        | false -> "· "
    | Exposed -> 
        match cell.SurroundingCount with
        | None 
        | Some 0 -> "  "
        | Some i -> i.ToString() + " "
    | Flagged -> "? "

let getRowText (game:Game) (idx:int) (row:Cell[]) = 
    let rowText = row |> Array.map (getCellChar game) |> String.concat "" 
    sprintf "%02i %s%02i" (idx + 1) rowText (idx + 1)

let inc x = x + 1
let getHeader (game:Game) (leftPadding:string) = 
    let width = game.Width
    let maxChars = width.ToString().Length
    let indexes = 
        [0..(width - 1)]
        |> Seq.map inc
        |> Seq.map (fun x -> x.ToString().PadLeft(maxChars, ' '))
        |> Array.ofSeq
    [0..(maxChars - 1)]
        |> List.map (fun i -> 
            indexes 
            |> Array.map (fun x -> x.[i].ToString())
            |> String.concat " ")
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
    printfn "%s" (getHeader game "   ")
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
                | "flag"::x::y::_ -> flag game ((int x) - 1) ((int y) - 1)
                | "sweep"::x::y::_ -> sweep game ((int x) - 1) ((int y) - 1)
                | x::y::_ -> sweep game ((int x) - 1) ((int y) - 1)
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

let gameloop randomNumbers =
    let mutable game = createEasyGame randomNumbers
    while game.State <> GameState.Exit && game.State <> GameState.Quit do
        game <- processMove game
    
    

[<EntryPoint>]
let main argv =
    let rand = new System.Random()
    let randomNumbers = [0..1000] |> List.map (fun _ -> rand.Next()) |> Array.ofList
    gameloop randomNumbers
    0