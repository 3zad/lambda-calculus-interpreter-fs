module Program

open Runner
open Evaluator
open Types
open LambdaCalculusInterpreter
open FParsec
open Expander

[<EntryPoint>]
let main _ = 
    // Fast multiplication of two huge numbers which would be computationally impossible with church encoding
    match execsFast Normal "x=486975092587932083745963507549530;main= sub x 98576978450049586794576983657098;" with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"

    // Fully-fledged example
    match execsFast Normal """
        % NB! Running in applicative mode is many times faster!

        % Define some constants
        ten = 10;
        twenty = 20;
        fifty = 50;
        hundred = 100;
        
        % Some arithmetic
        doubleTen = mul ten (succ ten); % 10 * 11 = 110
        quadrupleTen = mul doubleTen (succ ten); % 110 * 11 = 1210 (will reduce later)
        halfHundred = sub hundred fifty; % 100 - 50 = 50

        % Nested arithmetic using constants
        result1 = add doubleTen halfHundred; % 110 + 50 = 160
        result2 = sub (mul ten twenty) (add fifty ten); % 200 - 60 = 140

        % Recursion doesn't work
        % liida = λ n m. cond (iszero n) m (liida (sub n 1) (add m 1));

        % Main method combines arithmetic and logical checks
        main = fact( add result2 (add (add result1 result2) (succ ten) ) ); % (140 + (160 + 140) + 11)! =~ 7.8175e29

    """ with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"
    0
