module Runner

open FParsec
open Types
open Prelude
open Parser
open Evaluator
open Expander

let parseProgram (s: string) : List<Statement> =
    match run statements s with
    | Success (stmts, _, _) -> checkRec stmts
    | Failure (msg, _, _) -> failwith msg

let run' (s: string) : Expression option =
    let fullProgram = header + s
    let expanded = expandAll (parseProgram fullProgram)
    getMain expanded