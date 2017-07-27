// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
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

let mutable game = createEasyGame (new System.Random())

[<EntryPoint>]
let main argv = 
    printGame game
    let e = System.Console.ReadLine()
    0 // return an integer exit code
