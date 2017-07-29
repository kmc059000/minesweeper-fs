open Minesweeper
open Common
open RandomSolver

let createGame i = 
    let rand = new System.Random()
    let randoms = ([0..1000] |> List.map (fun _ -> rand.Next()) |> Array.ofList)
    createEasyGame randoms 

let games = [0..10000] |> List.map createGame |> Array.ofList

let testSolver (solver:Game->Solution) game = async {
    let solved = solver game
    return solved.SolutionState
}

let rec runSolverTests (solver:Game->Solution) numTests previousResults =
    games
    |> Seq.map (testSolver solver)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.fold (fun acc elm -> 
        let successes, total = acc
        match elm with
            | Win -> (successes + 1, total + 1)
            | Dead -> (successes, total + 1)
            | _ -> failwith "Unexpected solution state") 
        (0,0)
    //match numTests with
    //| 0 -> previousResults
    //| _ -> 
    //    let rand = new System.Random()
    //    let randoms = ([0..1000] |> List.map (fun _ -> rand.Next()) |> Array.ofList)
    //    let runResult = createEasyGame randoms |> testSolver solver
    //    let successes, total = previousResults
    //    let newResults =
    //        match runResult with
    //        | Win -> (successes + 1, total + 1)
    //        | Dead -> (successes, total + 1)
    //        | _ -> failwith "Unexpected solution state"
    //    runSolverTests solver (numTests - 1) newResults

let testRandomSolver = runSolverTests randomSolver 10000 (0,0)

[<EntryPoint>]
let main argv = 
    printf "%s" (testRandomSolver.ToString())
    let r = System.Console.ReadLine()
    0
