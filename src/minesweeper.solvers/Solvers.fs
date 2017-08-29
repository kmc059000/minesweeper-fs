module Solvers

open Common

let probabilitySolver = Game.solve ProbabilitySolver.solve
let betterProbabilitySolver = Game.solve BetterProbabilitySolver.solve
let randomSolver = Game.solve RandomSolver.solve
