module Types

// Lambda expression
type Expression = 
    | Lambda of string * Expression
    | Application of Expression * Expression
    | Variable of string
    | Natural of bigint

let rec exprToString (expr: Expression) : string =
    match expr with
    | Variable x -> x
    | Natural n -> n.ToString()
    | Lambda (x, e) -> sprintf "λ%s. (%s)" x (exprToString e)
    | Application (f, a) -> sprintf "(%s) (%s)" (exprToString f) (exprToString a)

// Reduction enum
type Reduction =
    | Normal
    | Applicative



// Syntactic sugar section

// Comments, assignments of top-level variables, and file imports
type Statement =
    | Comment of string
    | Assign of string * Expression
    | Import of string
    | Execute of Expression

