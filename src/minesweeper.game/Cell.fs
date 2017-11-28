namespace Cells

open Coordinates

type CellState = Hidden | Exposed | Flagged

type Cell = {
    Coords: Coordinate;
    IsMine: bool;
    SurroundingCount: int option;
    TotalNeighbors: int;
};

module Cells =
    let create state coords isMine surroundingCount totalNeighbors =
        {
            Coords = coords; 
            IsMine = isMine; 
            SurroundingCount = surroundingCount;
            TotalNeighbors = totalNeighbors
        };
        
    let private getSurroundingCount mineLocations cell =
        cell.Coords
        |> Coordinates.getValidSurroundingIndexes
        |> Set.intersect mineLocations
        |> Seq.length

    let withSurroundingCount mineLocations cell =
        { cell with SurroundingCount = Some (getSurroundingCount mineLocations cell) }
    
    let isMine c = c.IsMine
    let isNotMine c = not c.IsMine

    let getIndex c = c.Coords.Index

module CellFactory =
    let initCell gameSize index =
        let coords = 
            {
                Index = index
                X = index % gameSize.Width
                Y = index / gameSize.Width
                GameSize = gameSize
            
            }
        let totalNeighbors = coords |> Coordinates.getValidSurroundingIndexes |> Seq.length

        {
            Coords = coords;
            IsMine = false;
            SurroundingCount = None;
            TotalNeighbors = totalNeighbors; 
        }
    

