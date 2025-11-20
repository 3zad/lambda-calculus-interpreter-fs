(* 
    * Author: Zachary Davis
    * Purpose: To create a transpiler and interpreter for lambda calculus
    * Date: November 10th, 2025
    * Copyright: This code is in the public domain; feel free to use, copy, modify, and distribute it without credit or attribution.
*) 

module LambdaCalculusInterpreter

open Types
open Evaluator
open Runner
open Prelude
open Substitution
open Expander


let exec (r: Reduction) (filePath: string) : unit =
    try
        let content = System.IO.File.ReadAllText(filePath)

        let stmts = parseProgram content
        let importedContent = expandImports stmts
        let fullProgram = pureHeader + importedContent + content

        match run' fullProgram with
        | Some e ->
            let evaluated = evalExpression r e
            match churchToN evaluated with
            | Some (Natural n) -> printfn "%A" n
            | Some other -> printfn "%A" other
            | None -> printfn "%A" evaluated
        | None ->
            printfn "Error during execution. Check syntax."
    with
    | ex -> printfn "Error reading file: %s" ex.Message

let transpile (inputFileName: string) (outputFileName: string) : unit =
    try
        let content = System.IO.File.ReadAllText(inputFileName)

        let stmts = parseProgram content 
        let importedContent = expandImports stmts
        let fullProgram = pureHeader + importedContent + content

        let exprs = expandAll (parseProgram fullProgram)
        let output =
            match getMain exprs with
            | Some e -> "main="+ exprToString e + ";" // Wrap it in an entry point
            | None -> "Error: No main expression found."

        System.IO.File.WriteAllText(outputFileName, output)

    with
    | ex ->
        System.IO.File.WriteAllText(outputFileName, $"Error reading input file: {ex.Message}")

let fullProgram (s: string) : Result<string, string> =
    try
        let stmts = parseProgram s
        let importedContent = expandImports stmts
        Result.Ok (pureHeader + importedContent + s)
    with ex ->
        Result.Error "Error during parsing"

let execs (r: Reduction) (s: string) : Result<string, string> =
    let fullProgram = fullProgram s

    match fullProgram with
        | Result.Ok fp ->
            match run' fp with
            | Some e ->
                let evaluated = evalExpression r e
                match churchToN evaluated with
                | Some (Natural n) -> Result.Ok (exprToString (Natural n))
                | Some other -> Result.Ok (exprToString other)
                | None -> Result.Ok (exprToString evaluated)
            | None ->
                Result.Error "Error during execution. Check syntax."
        | Result.Error err ->
            Result.Error err

let execsFast (r: Reduction) (s: string) : Result<string, string> =

    let rec runEverythingInProgram (ss: List<Statement>) : Result<string, string> =
        match ss with
        | [] -> Result.Ok "Done executing"
        | s::ss -> 
            match s with
            | Execute e -> 
                fastEvalExpression r e |> ignore
                runEverythingInProgram ss
            | _ -> runEverythingInProgram ss

    let fullProgram = fullProgram s
    match fullProgram with
    | Result.Ok fp -> runEverythingInProgram (runFast fp)
    | Result.Error err -> Result.Error err

let transpiles (s: string) : Result<string, string> =
    let fullProgram = fullProgram s

    match fullProgram with
    | Result.Ok fp ->
        let exprs = expandAll (parseProgram fp)
        match getMain exprs with
        | Some e -> Result.Ok ("main="+ exprToString e + ";") // Wrap it in an entry point
        | None -> Result.Error ("Error: No main expression found.")
    | Result.Error err -> Result.Error err

// Convenience functions which are C#-friendly
let execsNormal (s: string) : string = 
    match execs Normal s with
    | Result.Ok s -> s
    | Result.Error err -> err

let execsApplicative (s: string) : string = 
    match execs Applicative s with
    | Result.Ok s -> s
    | Result.Error err -> err

let execsFastNormal (s: string) : string = 
    match execsFast Normal s with
    | Result.Ok s -> s
    | Result.Error err -> err

let execsFastApplicative (s: string) : string = 
    match execsFast Applicative s with
    | Result.Ok s -> s
    | Result.Error err -> err

let transpilesCs (s: string) : string =
    match transpiles s with
    | Result.Ok s -> s
    | Result.Error err -> err