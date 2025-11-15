module Program

open Runner
open Evaluator
open Types
open LambdaCalculusInterpreter
open FParsec
open Expander

[<EntryPoint>]
let main _ = 
    // Fast multiplication of two huge numbers which would be computationally impossible with church encoding
    match execsFast Normal "x=486975092587932083745963507549530;main= sub x 98576978450049586794576983657098;" with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"

    // Factorial of 10000 (huuuge number)
    match execsFast Normal "x=10000;main= fact x;" with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"
    0
