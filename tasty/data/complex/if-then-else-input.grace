# This checks what type would be inferred for a first-class function wrapping
# the `if`/`then`/`else` keywords
\x -> \y -> \z -> if x then y else z
