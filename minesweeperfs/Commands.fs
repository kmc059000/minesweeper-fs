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

let getSurroundingCellsToSweep index game =
    let cell = game.Cells.[index]
    match cell.State with
    | Hidden-> 
        getValidSurroundingIndexes game.Width game.Height cell
            |> Seq.map (getCell game)
            |> Seq.filter (fun x -> x.IsMine = false)
            |> Seq.map (fun x -> x.Coords.Index)
            |> List.ofSeq
    | _ -> []
        
//this will auto-sweep the surrounding cells if the sweeped cell has 0 surrounding mines.
let rec sweepCells indexes game =
    match indexes with 
    | [] -> game
    | x::xs ->
        let cell = game.Cells.[x]
        let surrounding =
            match cell.SurroundingCount with
            | Some 0 -> getSurroundingCellsToSweep x game
            | _ -> []
        let newGame = game |> setGameCellState x Exposed
        newGame |> sweepCells surrounding |> sweepCells xs
        

let sweep (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    game 
        |> tryPlaceMines index
        |> sweepCells [index]
        |> testWin
        |> testLoss index

let flag (game:Game) (x:int) (y:int) = 
    let index = x + (y * game.Width)
    game |> setGameCellState index Flagged
