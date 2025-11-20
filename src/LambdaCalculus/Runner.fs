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
    let fullProgram = pureHeader + s
    let expanded = expandAll (parseProgram fullProgram)
    getMain expanded

let runFast (s: string) : List<Statement> =
    let fullProgram = fastHeader + s
    let expanded = expandAllNoChurch (parseProgram fullProgram)
    expanded