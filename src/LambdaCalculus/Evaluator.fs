module Evaluator

open Types
open Parser
open Substitution
open Prelude

let rec evalExpression (r: Reduction) (e: Expression) =

    // Beta reduction helper function
    let rec reduce (r: Reduction) (e: Expression) : Expression option =
        match e with
        | Application (Lambda(x, body), arg) ->
            match r with
            | Normal -> Some (substitute x arg body)
            | Applicative ->
                match reduce r arg with
                | Some arg' -> Some (Application(Lambda(x, body), arg'))
                | None -> Some (substitute x arg body)
        | Application (f, a) ->
            match r with
            | Normal ->
                match reduce r f with
                | Some f' -> Some (Application(f', a))
                | None ->
                    match reduce r a with
                    | Some a' -> Some (Application(f, a'))
                    | None -> None
            | Applicative ->
                match reduce r f with
                | Some f' -> Some (Application(f', a))
                | None ->
                    match reduce r a with
                    | Some a' -> Some (Application(f, a'))
                    | None -> None

        | Lambda (x, body) ->
            match reduce r body with
            | Some body' -> Some (Lambda(x, body'))
            | None -> None

        | _ -> None

    match reduce r e with
    | Some (e') -> evalExpression r e'
    | None -> e

let rec expandAll (stmts: List<Statement>) : List<Statement> =

    let rec expandVars (env: List<Statement>) (e: Expression) : Expression =

        let rec lookupAssign (s: string) (stmts': List<Statement>) : Expression option =
            match stmts' with
            | [] -> None
            | (stmt :: stmts') ->
                match stmt with
                | Assign (name, e) ->
                    if name.Equals(s) then Some e else lookupAssign s stmts'
                | Import _ -> lookupAssign s stmts'
                | Comment _ -> lookupAssign s stmts'

        match e with
        | Variable x ->
            match lookupAssign x env with
            | Some (Natural n) -> nToChurch n
            | Some (e) -> expandVars env e
            | None -> Variable x // Free variable
        | Lambda (x, body) -> Lambda (x, (expandVars env body))
        | Application (f, a) -> Application (expandVars env f, expandVars env a)
        | Natural n -> Natural n

    let rec expandAllReversed (stmts': List<Statement>) : List<Statement> =
        match stmts' with
        | [] -> []
        | (Assign (name', e))::stmts' -> Assign (name', (convertNaturals ( expandVars (Assign (name', e) :: stmts') (Variable name')))) :: expandAllReversed stmts'
        | stmt::stmts' -> stmt :: expandAllReversed stmts'

    List.rev (expandAllReversed (List.rev stmts))


let rec expandImports (stmt: List<Statement>) : string =
    match stmt with
    | [] -> ""
    | stmt :: stmts ->
        match stmt with  
        | Import fileName ->
            let content = System.IO.File.ReadAllText(fileName + ".lam", System.Text.Encoding.UTF8)
            content + expandImports stmts

        | _ -> expandImports stmts