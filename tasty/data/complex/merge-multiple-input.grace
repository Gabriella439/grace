# This checks that the type-checker correctly infers that the input type of the
# `X` handler is `Natural`, by working backwards from the result type of the `Y`
# handler
merge { X: \x -> x, Y: \_ -> 2 }
