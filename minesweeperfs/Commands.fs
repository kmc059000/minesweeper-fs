module Commands

open Minesweeper

let setCellState (selectedCell:Cell) state (cell:Cell) = 
    match cell = selectedCell with
        | true -> { cell with State = state}
        | false -> cell

let isWin (cells:Cell[]) = 
    cells
    |> Array.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
    |> not

let setGameCellState selectedCell state (game:Game) =
    let newCells = game.Cells |> Array.map (setCellState selectedCell state)
    { game with Cells = newCells}

let testWin (game:Game) =
    let isWin = isWin game.Cells
    match isWin with
    | true -> { game with State = Win }
    | false -> game

let testLoss selectedCell (game:Game) = 
    match selectedCell.IsMine with
    | true -> { game with State = GameState.Dead; }
    | false -> game


let sweep (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    let selectedCell = game.Cells.[index]
    game 
        |> setGameCellState selectedCell Exposed
        |> tryPlaceMines selectedCell
        |> testWin
        |> testLoss selectedCell

let flag (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    let cell = game.Cells.[index]
    game |> setGameCellState cell Flagged