open Games
open Common.Solvers
open RandomSolver
open ProbabilitySolver

type SolutionStats = 
    { TotalWins : int; TotalLosses: int; TotalGames: int; LossProbabilities: float option list}
    with member this.WinPercent = (float this.TotalWins) / (float this.TotalGames )

type SolutionLossStats = 
    { 
        AvgProbability: float;
        MinProbability: float;
        MaxProbability: float;
    }

module SolutionStats =
    let empty = { TotalWins = 0; TotalLosses = 0; TotalGames = 0; LossProbabilities = [] }

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
    
    let getLossStats stats =
        let ps = 
            stats.LossProbabilities 
            |> List.choose id

        match ps with
        | [] -> None
        | _ -> 
            Some { 
                AvgProbability = ps |> Seq.average;
                MinProbability = ps |> Seq.min;
                MaxProbability = ps |> Seq.max;
            }
    

let rand = new System.Random()
let createGame i = 
    (GameFactory.createEasyGame (rand.Next()), i)

let testCases = 1000
let games = [0..(testCases - 1)] |> Seq.map createGame

let testSolver (solver:Game->Solution) game = solver game

let testSolverAsync (solver:Game->Solution) (game,i) = async {
    if i % 1000 = 0 then printf "."
    return testSolver solver game
}

let runSolverTests (solver:Game->Solution) =
    games
    |> Seq.map (testSolverAsync solver)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.fold (fun stats solution -> 
        match solution.SolutionState with
            | Win -> stats |> SolutionStats.addWin
            | Dead -> stats |> SolutionStats.addLoss solution
            | _ -> failwith "Unexpected solution state") 
        SolutionStats.empty

let printLossStats stats = 
    printfn "Avg Loss Last Sweep %%: %f%%" stats.AvgProbability
    printfn "Min Loss Last Sweep %%: %f%%" stats.MinProbability
    printfn "Max Loss Last Sweep %%: %f%%" stats.MaxProbability

let printResults name results =
    printfn "%s Results" name
    printfn "Wins: %i" results.TotalWins
    printfn "Losses: %i" results.TotalLosses
    printfn "Win Percent: %f%%" results.WinPercent
    let lossStats = SolutionStats.getLossStats results
    match lossStats with
    | Some s -> printLossStats s
    | None -> ()
    printfn ""

let testRandomSolver = runSolverTests randomSolver
let testProbabilitySolver = runSolverTests probabilitySolver

[<EntryPoint>]
let main argv = 
    printfn "" 
    printResults "Random" testRandomSolver
    printResults "Probability" testProbabilitySolver
    let r = System.Console.ReadLine()
    0
