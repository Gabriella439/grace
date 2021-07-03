\noMiddle ->
\doubleNegation ->
  merge
    { "Left": \x -> x, "Right": \x -> merge { } (doubleNegation x) }
    noMiddle
