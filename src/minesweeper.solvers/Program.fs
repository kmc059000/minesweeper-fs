open Games
open Common
open RandomSolver
open ProbabilitySolver
open Results


let rand = new System.Random()
let createGame i = 
    (GameFactory.createMediumGame (rand.Next()), i)

printfn "How many games to test?"
let testCases = System.Console.ReadLine() |> int
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
        stats
        |> SolutionStats.addSolution solution
        |> SolutionStats.addSweepCounts solution
        |> SolutionStats.addCellCounts solution) 
        SolutionStats.empty

[<EntryPoint>]
let main argv = 
    printfn "" 
    SolutionStats.printResults "Random" (runSolverTests randomSolver)
    SolutionStats.printResults "Probability" (runSolverTests probabilitySolver)
    let r = System.Console.ReadLine()
    0
