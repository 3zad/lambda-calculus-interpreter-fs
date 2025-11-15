module Math

// No recursion so we don't have to worry about the stack
let factorial (n: bigint) : bigint =
    let mutable result = 1I
    let mutable counter = n
    while counter > 0I do
        result <- result * counter
        counter <- counter - 1I
    result

let power (n: bigint) (m: bigint) : bigint =
    let base' = n
    let mutable counter = m
    let mutable product = 1I
    while m > 0I do
        product <- product * base'
        counter <- counter - 1I
    product