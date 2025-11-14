module Parser

open Types
open FParsec

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

module public ExprParser =
    let expr, exprRef = createParserForwardedToRef<Expression, unit>()

    // Parser for lambda expressions
    let lam = 
        (lexeme (pchar 'λ' <|> pchar '\\')) >>.
        (many1 (lexeme name)) .>>
        lexeme (pchar '.') >>= fun xs ->
        expr
        |>> fun body -> List.foldBack (fun x acc -> Lambda(x, acc)) xs body

    // Atomic values
    let atom =
        lexeme (
            (parens expr) 
            <|> (name |>> Variable) 
            <|> (integer |>> Natural)
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

module public StmtParser =
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
        spaces >>. (
            attempt comment
            <|> attempt import
            <|> assign
        )


(* 
    Each program is made of many statements. Each statement only contains one lambda-calculus 
    expression max, so a similar function is not needed for the expression parser.
*)
let public statements : Parser<Statement list, unit> =
    many (StmtParser.stmt .>> spaces)