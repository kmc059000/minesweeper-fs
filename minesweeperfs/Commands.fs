module Commands

open Minesweeper

let setCellState index state (cell:Cell) = 
    match cell.Coords.Index = index with
        | true -> { cell with State = state}
        | false -> cell

let isWin (cells:Cell[]) = 
    cells
    |> Array.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
    |> not

let setGameCellState index state (game:Game) =
    let newCells = game.Cells |> Array.map (setCellState index state)
    { game with Cells = newCells}

let testWin (game:Game) =
    let isWin = isWin game.Cells
    match isWin with
    | true -> { game with State = Win }
    | false -> game

let testLoss index (game:Game) = 
    let cell = game.Cells.[index]
    match cell.IsMine with
    | true -> { game with State = GameState.Dead; }
    | false -> game


let sweep (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    game 
        |> tryPlaceMines index
        |> setGameCellState index Exposed
        |> testWin
        |> testLoss index

let flag (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    game |> setGameCellState index Flagged