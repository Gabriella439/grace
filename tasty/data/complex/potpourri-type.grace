forall (a : Type) .
forall (b : Type) .
forall (c : Type) .
forall (d : Fields) .
  (b -> a) -> { a: List c -> b, b: c, c: c, d } -> a
