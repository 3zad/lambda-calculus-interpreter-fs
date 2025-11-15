module Substitution

open Types

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
    | Variable z -> if z = x then Variable y else Variable z
    | Lambda (z, body) -> 
        if z = x then Lambda (y, rename x y body)
        else Lambda (z, rename x y body)
    | Application (f, a) -> Application (rename x y f, rename x y a)
    | Natural n -> Natural n

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
        | Some n -> Some (Natural (bigint n))
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
    | Natural n -> nToChurch (int n)
    | Variable x -> Variable x
    | Lambda (x, body) -> Lambda (x, convertNaturals body)
    | Application (f, a) -> Application (convertNaturals f, convertNaturals a)