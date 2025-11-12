(* 
    * Author: Zachary Davis
    * Purpose: To create a transpiler and interpreter for lambda calculus
    * Date: November 10th, 2025
    * Copyright: This code is in the public domain; feel free to use, copy, modify, and distribute it without credit or attribution.
*) 

open FParsec




// ---------- DATA SECTION ---------- //

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




// ---------- PARSER SECTION ---------- //

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
        spaces >>. (
            attempt comment
            <|> attempt import
            <|> assign
        )

// List of freevars in a given expression
let rec freeVars (e: Expression) : List<string> =
    match e with
    | Variable (y) -> [y]
    | Lambda (y, body) -> freeVars body |> List.filter (fun z -> z <> y)
    | Application (f, a) -> freeVars f @ freeVars a
    | Natural (_) -> []

(* 
    Generate a fresh variable, if in the free variables list.
    Free variables can be named anything and it won't affect the program.
*)
let rec freshVar (ss: List<string>) (y: string) : string =
    if List.contains y ss then freshVar ss (y + "'")
    else y

// Rename all the variables, bounded or not, in a certain expression.
let rec rename (x: string) (y: string) (e: Expression) : Expression =
    match e with
    | Variable (z) -> if z.Equals(x) then Variable (y) else Variable (x)
    | Lambda (z, body) -> if z.Equals(x) then Lambda(y, rename x y body) else Lambda(x, rename x y body)
    | Application (f, z) -> Application (rename x y f, rename x y z)
    | Natural (n) -> Natural (n)

// Substitution
let rec substitute (x: string) (arg: Expression) (e: Expression) : Expression =
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
    | Natural (n) -> Natural (n)

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

// Church expressions for natural numbers
let nToChurch (n: int) : Expression =
    let f = "f"
    let x = "x"
    
    // Helper function
    let rec buildApps (n': int) (base': Expression) (s: string) =
        match n' with
        | 0 -> base'
        | nat -> Application (Variable f, buildApps (nat - 1) base' f)

    let body = buildApps n (Variable (x)) f

    Lambda (f, (Lambda (x, body)))

// Delta reduction
let rec churchToN (e: Expression) : Expression option =

    // Helper function
    let rec countApps (e': Expression) (f: string) (x: string) : int option =
        match e' with
        | Variable x' -> if x'.Equals(x) then Some 0 else None
        | Application (Variable f', rest) -> 
            if f'.Equals(f) then
                match countApps rest f x with
                     | Some n -> Some (n + 1)
                     | None -> None
            else None
        | _ -> None

    match e with
    | Lambda (f, Lambda (x, body)) ->
        match countApps body f x with
        | Some n -> Some (Natural n)
        | None -> Some body
    | Application (f, a) ->
        match churchToN f with
        | Some (Natural n) -> Some (Natural n)
        | Some other -> Some other
        | None -> churchToN a
    | _ -> Some (e)

// Convert all natural numbers to church encoding for pure lambda calculus.
let rec convertNaturals (e: Expression) : Expression =
    match e with
    | Natural n -> nToChurch n
    | Variable x -> Variable x
    | Lambda (x, body) -> Lambda (x, convertNaturals body)
    | Application (f, a) -> Application (convertNaturals f, convertNaturals a)

(* 
    Each program is made of many statements. Each statement only contains one lambda-calculus 
    expression max, so a similar function is not needed for the expression parser.
*)
let statements : Parser<Statement list, unit> =
    many (StmtParser.stmt .>> spaces)

let parseProgram (s: string) : List<Statement> =
    match run statements s with
    | Success (stmts, _, _) -> checkRec stmts
    | Failure (msg, _, _) -> failwith msg




// ---------- EVALUATION SECTION ---------- //

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

let rec getMain (stmts: List<Statement>) : Expression option =
    match stmts with
    | [] -> None
    | (Assign (name', e))::stmts -> if name'.Equals("main") then Some e else getMain stmts
    | _::stmts -> getMain stmts

let header : string =
    $"
    pred = \\ n. \\ f. \\ x. n (\\ g. \\ h. h (g f)) (\\ u. x) (\\ u. u);
    add = \\ n m. \\ f x. n f (m f x);
    sub = \\ m. \\ n. n pred m;
    mul = \\ m. \\ n. \\ f. m (n f);
    true = \\ x. \\y. x;
    false = \\ x. \\ y. y;
    iszero = \\ n. n ( \\ x. false ) true;
    cond = \\b. \\t. \\e. b t e;
    nil = \\x.true;
    cons = \\x.\\l.\\c.\\n. c x (l c n);
    hd = \\p. p (\\h t. h) nil;
    tl = \\p. p (\\h t. t) nil;
    null = \\p. p (\\h t. false) true;
    Y = \\f. (\\x. f (x x)) (\\x. f (x x));
    "

let run' (r: Reduction) (s: string) : Expression option =
    let fullProgram = header+s
    let expanded = expandAll (parseProgram fullProgram)
    match getMain expanded with
    | Some e -> 
        let evaluated = evalExpression r e
        match churchToN evaluated with
        | Some n -> Some n
        | None -> Some evaluated

    | None -> None

let helper s =
    let f = header + s
    match getMain (expandAll (parseProgram f)) with
        | Some x -> Some (evalExpression Normal x)
        | None -> None

printfn "%A" (run' Applicative "kolm = sub (add (mul 2 2) 3) 4;main = add kolm (add kolm kolm);")