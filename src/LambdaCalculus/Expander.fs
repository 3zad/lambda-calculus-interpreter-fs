module Expander

open Types
open Substitution

// Y combinator wrap
let rec checkRec (stmts: List<Statement>) : List<Statement> = 
    match stmts with
    | [] -> []
    | (Import (s)) :: stmts -> [Import s] @ checkRec stmts
    | (Comment (s)) :: stmts -> checkRec stmts
    | (Assign (name', e)) :: stmts ->
        let eFinal =
            if List.contains name' (freeVars e) then
                Application ((Variable "Y"), (Lambda ("self", (rename name' "self" e))))
            else
                e
        (Assign (name', eFinal)) :: checkRec stmts
    | (Execute (e)) :: stmts -> [Execute e] @ checkRec stmts


let rec getMain (stmts: List<Statement>) : Expression option =
    match stmts with
    | [] -> None
    | (Assign (name', e))::stmts -> if name'.Equals("main") then Some e else getMain stmts
    | _::stmts -> getMain stmts