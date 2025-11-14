module Tests

open Xunit
open Types
open Substitution
open Evaluator

// ---------- Beta Reduction Tests ---------- //
[<Fact>]
let ``freeVars returns correct list`` () =
    let expr = Lambda("x", Application(Variable "x", Variable "y"))
    let result = freeVars expr
    Assert.Equal<string list>(["y"], result)

[<Fact>]
let ``substitute replaces correctly`` () =
    let expr = Application(Variable "x", Variable "y")
    let replaced = substitute "x" (Variable "z") expr
    Assert.Equal("(z) (y)", exprToString replaced)


// ---------- Evaluation Tests ---------- //
[<Fact>]
let ``eval simple identity`` () =
    let expr = Application(Lambda("x", Variable "x"), Variable "y")
    let result = evalExpression Normal expr
    Assert.Equal("y", exprToString result)

[<Fact>]
let ``eval nested reduction`` () =
    let expr = Application(Lambda("x", Application(Variable "x", Variable "x")), Lambda("x", Variable "x"))
    let result = evalExpression Normal expr
    Assert.Equal("λx. (x)", exprToString result)



// ---------- Church numeral tests ---------- //
[<Fact>]
let ``convert 2 to church and back`` () =
    let church2 = nToChurch 2
    let back = churchToN church2
    match back with
    | Some (Natural 2) -> Assert.True(true)
    | _ -> Assert.Fail("Church numeral conversion failed")