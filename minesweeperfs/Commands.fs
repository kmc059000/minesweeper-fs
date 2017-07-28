module Commands

open Minesweeper

let sweep (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    let cell = game.Cells.[index]
    
    let processCell cell' =
        match cell' = cell with
        | true -> { cell' with State = Exposed}
        | false -> cell'

    let newCells = 
        game.Cells
        |> Array.map processCell
    
    let isWin = 
        newCells
        |> Array.exists (fun x -> not x.IsMine && x.State <> CellState.Exposed)
        |> not
    
    match cell.IsMine with
    | true -> { game with State = GameState.Dead; Cells = newCells }
    | false -> 
        let newState = if isWin then GameState.Win else GameState.Playing
        { game with State = newState; Cells = newCells }

let flag (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    let cell = game.Cells.[index]
    
    let processCell cell' =
        match cell' = cell with
        | true -> { cell' with State = Flagged}
        | false -> cell'

    let newCells = 
        game.Cells
        |> Array.map processCell
    
    { game with State = GameState.Playing; Cells = newCells }