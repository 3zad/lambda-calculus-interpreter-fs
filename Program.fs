(* 
    * Author: Zachary Davis
    * Purpose: To create a transpiler and interpreter for lambda calculus
    * Date: November 10th, 2025
    * Copyright: This code is in the public domain; feel free to use, copy, modify, and distribute it without credit or attribution.
*) 

open FParsec

// Lambda expression
type Expr = 
    | Lambda of string * Expr
    | Application of Expr * Expr
    | Variable of string
    | Constant of int

// Top-level variables and a pure lambda-calculus expression
type Global =
    | Def of string
    | Pure of Expr

// Comments, assignments of top-level variables, and file imports
type Statement =
    | Comment of string
    | Assign of string * Expr
    | Import of string

// Reduction enum
type Reduction =
    | Normal
    | Applicative

// Helper function for dealing with whitespaces
let lexeme p = p .>> spaces

// Integer
let integer : Parser<int, unit> =
    pint32 .>> spaces

// Returns the content inside parenthesis
let parens p : Parser<'a, unit> =
    lexeme (pchar '(') >>.
    p
    .>> lexeme (pchar ')')

// Helper parser for variable names. Valid variable names: hello, hello2, h2ello, h_ello, h_2, h2_
let variableName : Parser<char, unit> = letter <|> digit <|> pchar '_'

// Lambda-calculus variables and top-level variables
let name : Parser<string, unit> =
    letter .>>. manyChars variableName
    |>> fun (c, s) -> string c + s

module ExprParser =
    let expr, exprRef = createParserForwardedToRef<Expr, unit>()

    // Parser for lambda expressions
    let lam = 
        (lexeme (pchar '\\' <|> pchar 'λ')) >>.
        (many1 (lexeme name)) .>>
        lexeme (pchar '.') >>= fun xs ->
        expr
        |>> fun body -> List.foldBack (fun x acc -> Lambda(x, acc)) xs body

    // Atomic values
    let atom =
        lexeme (
            (parens expr) 
            <|> (name |>> Variable) 
            <|> (integer |>> Constant)
        )

    // Parser for applications
    let app =
        many1 atom
        |>> fun es -> List.reduce (fun acc e -> Application(acc, e)) es

    do exprRef :=
        lam <|> app

// Helper parser for import names.
(*
    * import from the same directory: import foo
    * import from subdirectory bar: import bar.foo
*)
let importName : Parser<string, unit> =
    letter >>= fun first ->
    manySatisfy (fun c -> c <> ';') >>= fun rest ->
    preturn (sprintf "%c%s" first (System.String.Concat rest))

module StmtParser =
    let stmt, stmtRef = createParserForwardedToRef<Statement, unit>()
        
    let comment =
        lexeme (pchar '%') >>.
        manySatisfy (fun c -> c <> '\n') .>>
        lexeme (pchar '\n')
        |>> Comment

    let import =
        lexeme (pstring "import") >>.
        importName .>>
        lexeme (pchar ';')
        |>> Import

    let assign =
        (lexeme name .>> lexeme (pchar '=')) .>>.
        (lexeme ExprParser.expr .>> lexeme (pchar ';'))
        |>> fun (varName, expr) -> Assign(varName, expr)

    do stmtRef :=
        comment <|> import <|> assign

let statements : Parser<Statement list, unit> =
    many (StmtParser.stmt .>> spaces)

// "hell_o42"
printfn "%A" (run name "hell_o42")

// "hell_o42"
printfn "%A" (run (parens name) "(hell_o42)")

// Variable "hell_o42"
printfn "%A" (run ExprParser.expr "(hell_o42)")

// Lambda ("x", Lambda ("y", Lambda ("z", Variable "x")))
printfn "%A" (run ExprParser.expr "\\x y z. x")

(* 
Application
  (Application
     (Application
        (Lambda ("x", Lambda ("y", Lambda ("z", Variable "x"))), Variable "j"),
      Variable "c"), Variable "v")
*)
printfn "%A" (run ExprParser.expr "(\\x y z. x) j c v")

// List of statements
(*
[
    Import "foo";
    Assign ("main", Application (Lambda ("x", Variable "x"), Variable "y"))
]
*)
printfn "%A" (run statements "import foo; main = (\\x. x) y;")

