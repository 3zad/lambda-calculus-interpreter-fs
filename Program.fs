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

(* 
    Each program is made of many statements. Each statement only contains one lambda-calculus 
    expression max, so a similar function is not needed for the expression parser.
*)
let statements : Parser<Statement list, unit> =
    many (StmtParser.stmt .>> spaces)

// List of freevars in a given expression
let rec freeVars (e: Expr) =
    match e with
    | Variable (y) -> [y]
    | Lambda (y, body) -> freeVars body |> List.filter (fun z -> z <> y)
    | Application (f, a) -> freeVars f @ freeVars a
    | Constant (_) -> []

(* 
    Generate a fresh variable, if in the free variables list.
    Free variables can be named anything and it won't affect the program.
*)
let rec freshVar (ss: List<string>) (y: string) =
    if List.contains y ss then freshVar ss (y + "'")
    else y

let rec rename (x: string) (y: string) (e: Expr) =
    match e with
    | Variable (z) -> if z.Equals(x) then Variable (y) else Variable (x)
    | Lambda (z, body) -> if z.Equals(x) then Lambda(y, rename x y body) else Lambda(x, rename x y body)
    | Application (f, z) -> Application (rename x y f, rename x y z)
    | Constant (n) -> Constant (n)


// Substitution
let rec substitute (x: string) (arg: Expr) (e: Expr) =
    match e with
    | Variable(y) -> if x.Equals(y) then arg else Variable(y)
    | Lambda(y, body) -> 
        if x.Equals(y) then Lambda(y, body)
        else 
            if List.contains y (freeVars arg) then
                freshVar (freeVars body @ freeVars arg) y
                |> fun y' ->
                    let result = Lambda (y', (substitute x arg (rename y y' body)))
                    result
            else
                Lambda (y, substitute x arg body)
    | Application (f, y) -> Application (substitute x arg f, substitute x arg y)
    | Constant (n) -> Constant (n)


// Beta reduction
let rec reduce (type': Reduction) (e: Expr) =
    match e with
    | (Application ((Lambda (x, body) ), arg)) -> 
        match type' with
        | Normal -> Some (substitute x arg body)
        | Applicative ->
            match reduce type' arg with
            | Some (arg') -> Some ( Application (Lambda (x, body), arg'))
            | None -> None
    | Application (f, x) -> 
        match reduce type' x with
        | Some x' -> Some (Application (f, x'))
        | None -> None 
    | Lambda (x, body) ->
        match reduce type' body with
        | Some (body') -> Some ( Lambda (x, body') )
        | None -> None
    | _ -> None