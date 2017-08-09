module Results

open Common

type SweepCounts = {
    Imperfect: int;
    Perfect: int;
}

type SolutionStats = 
    { 
        TotalWins : int; 
        TotalLosses: int; 
        TotalGames: int; 
        LossProbabilities: float option list;
        CellCounts: CellCounts list;
        SweepCounts: SweepCounts list
        TotalImperfectSweeps: int;
        TotalPerfectSweeps: int;
    }
    with member this.WinPercent = 100.0 * (float this.TotalWins) / (float this.TotalGames )

type SolutionLossStats = 
    { 
        AvgProbability: float;
        MinProbability: float;
        MaxProbability: float;
    }

type CellCountStats = 
    { 
        AvgExposed: float;
        AvgFlagged: float;
        AvgHidden: float;
    }

module SolutionStats =
    let empty = { 
        TotalWins = 0;
        TotalLosses = 0;
        TotalGames = 0;
        LossProbabilities = [];
        CellCounts = [];
        SweepCounts = [];
        TotalImperfectSweeps = 0;
        TotalPerfectSweeps = 0; 
    }

    let addWin stats = 
        { stats with 
            TotalWins = stats.TotalWins + 1;
            TotalGames = stats.TotalGames + 1; 
        }

    let addLoss solution stats = 
        { stats with 
            TotalLosses = stats.TotalLosses + 1; 
            TotalGames = stats.TotalGames + 1; 
            LossProbabilities = solution.LastProbability :: stats.LossProbabilities;
        }
    
    let addSolution solution stats =
        match solution.SolutionState with
        | Win -> stats |> addWin
        | Dead -> stats |> addLoss solution
        | _ -> failwith "Unexpected solution state"

    let getLossStats stats =
        let ps = 
            stats.LossProbabilities 
            |> List.choose id
        

        match ps with
        | [] -> None
        | _ -> 
            Some { 
                AvgProbability = ps |> Seq.average |> (*) 100.0;
                MinProbability = ps |> Seq.min |> (*) 100.0;
                MaxProbability = ps |> Seq.max |> (*) 100.0;
            }

    let getCellCountStats stats =
        { 
            AvgExposed = stats |> Seq.map (fun x -> float x.Exposed) |> Seq.average;
            AvgFlagged =  stats |> Seq.map (fun x -> float x.Flagged) |> Seq.average;
            AvgHidden =  stats |> Seq.map (fun x -> float x.Hidden) |> Seq.average;
        }
            
    let addSweepCounts solution stats =
        { stats with
            TotalImperfectSweeps = stats.TotalImperfectSweeps + solution.ImperfectSweeps
            TotalPerfectSweeps = stats.TotalPerfectSweeps + solution.PerfectSweeps;
            SweepCounts = { Perfect = solution.PerfectSweeps; Imperfect = solution.ImperfectSweeps } :: stats.SweepCounts
        }

    let addCellCounts solution stats =
        { stats with CellCounts = Solution.cellCounts solution :: stats.CellCounts }

    
    let printCellStats stats =
        printfn "Avg Hidden: %03f" stats.AvgHidden
        printfn "Avg Exposed: %03f" stats.AvgExposed
        printfn "Avg Flagged: %03f" stats.AvgFlagged

    let printLossStats stats = 
        printfn "Avg Loss Last Sweep %%: %f%%" stats.AvgProbability
        printfn "Min Loss Last Sweep %%: %f%%" stats.MinProbability
        printfn "Max Loss Last Sweep %%: %f%%" stats.MaxProbability

    let printSweepStats stats =
        printfn "Perfect Sweeps: %i" stats.TotalPerfectSweeps
        printfn "Imperfect Sweeps: %i" stats.TotalImperfectSweeps

        let avgPerfectSweeps = stats.SweepCounts |> Seq.map (fun x -> float x.Perfect) |> Seq.average
        let avgImperfectPerfectSweeps = stats.SweepCounts |> Seq.map (fun x -> float x.Imperfect) |> Seq.average

        printfn "Avg Perfect Sweeps per Game: %f" avgPerfectSweeps
        printfn "Avg Imperfect Sweeps per Game: %f" avgImperfectPerfectSweeps

    let printResults name results =
        printfn ""
        printfn "%s Results" name
        printfn "Wins: %i" results.TotalWins
        printfn "Losses: %i" results.TotalLosses
        printfn "Win Percent: %f%%" results.WinPercent
        printSweepStats results
        printCellStats (getCellCountStats results.CellCounts)
        let lossStats = getLossStats results
        match lossStats with
        | Some s -> printLossStats s
        | None -> ()
        printfn ""
        printfn ""