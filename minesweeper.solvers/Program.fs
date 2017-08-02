open Games
open Common.Solvers
open RandomSolver
open ProbabilitySolver

let rand = new System.Random()
let createGame i = 
    (GameFactory.createEasyGame (rand.Next()), i)

let testCases = 10000
let games = [0..(testCases - 1)] |> Seq.map createGame

let testSolver (solver:Game->Solution) game = 
    let solved = solver game
    solved.SolutionState

let testSolverAsync (solver:Game->Solution) (game,i) = async {
    if i % 1000 = 0 then printf "."
    return testSolver solver game
}

let rec runSolverTests (solver:Game->Solution) previousResults =
    games
    |> Seq.map (testSolverAsync solver)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.fold (fun acc elm -> 
        let successes, total = acc
        match elm with
            | Win -> (successes + 1, total + 1)
            | Dead -> (successes, total + 1)
            | _ -> failwith "Unexpected solution state") 
        (0,0)



let testRandomSolver = runSolverTests randomSolver (0,0)
let testProbabilitySolver = runSolverTests probabilitySolver (0,0)

[<EntryPoint>]
let main argv = 
    printfn "" 
    printfn "Random: %s %f%%" (testRandomSolver.ToString()) ((float <| fst testRandomSolver) / (float <| snd testRandomSolver))
    printfn "Probability Based: %s %f%%" (testProbabilitySolver.ToString()) ((float <| fst testProbabilitySolver) / (float <| snd testProbabilitySolver))
    let r = System.Console.ReadLine()
    0
