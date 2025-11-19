module Prelude

let pureHeader : string =
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

let fastHeader : string =
    $"
    Y = \\f. (\\x. f (x x)) (\\x. f (x x));
    "