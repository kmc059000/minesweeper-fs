open Minesweeper
open Common
open RandomSolver

let createGame i = 
    let rand = new System.Random()
    let randoms = ([0..1000] |> List.map (fun _ -> rand.Next()) |> Array.ofList)
    (createEasyGame randoms, i)

let games = [0..10000] |> Seq.map createGame

let testSolver (solver:Game->Solution) game = 
    let solved = solver game
    solved.SolutionState

let testSolverAsync (solver:Game->Solution) (game,i) = async {
    if i % 1000 = 0 then printf "."
    return testSolver solver game
}

let rec runSolverTests (solver:Game->Solution) numTests previousResults =
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

let testRandomSolver = runSolverTests randomSolver 10000 (0,0)

[<EntryPoint>]
let main argv = 
    printf "%s" (testRandomSolver.ToString())
    let r = System.Console.ReadLine()
    0
