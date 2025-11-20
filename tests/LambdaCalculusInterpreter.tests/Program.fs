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
    match execsFast Normal "x=486975092587932083745963507549530;result = mul x 98576978450049586794576983657098;print result;" with
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

        % Recursion
        addRec = λ n m. cond (iszero n) m (addRec (sub n 1) (add m 1));

        print (add 2 2);

        h = fact( add result2 (add (add result1 result2) (addRec (succ ten) (3)) ) ); % (140 + (160 + 140) + (11 + 3))! =~ 7.2670e1101
        %print h;

    """ with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"

    // Recursion testing
    match execsFast Normal """
        print (add 1 2);
        numInput = input;
        cond (iszero 1) 1 (2);
    """ with
    | Result.Ok x -> printfn "%A" x
    | Result.Error _ -> printfn "Error"

    0
