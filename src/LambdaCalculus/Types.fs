module Types

// Lambda expression
type Expression = 
    | Lambda of string * Expression
    | Application of Expression * Expression
    | Variable of string
    | Natural of int

// Top-level variables and a pure lambda-calculus expression
type Global =
    | Def of string
    | Pure of Expression

// Comments, assignments of top-level variables, and file imports
type Statement =
    | Comment of string
    | Assign of string * Expression
    | Import of string

// Reduction enum
type Reduction =
    | Normal
    | Applicative


let rec exprToString (expr: Expression) : string =
    match expr with
    | Variable x -> x
    | Natural n -> n.ToString()
    | Lambda (x, e) -> sprintf "λ%s. (%s)" x (exprToString e)
    | Application (f, a) -> sprintf "(%s) (%s)" (exprToString f) (exprToString a)