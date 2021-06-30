# This test force the interpret to unify two record types, one of which uses
# explicit polymorphism and the other of which uses implicit polymorphism
\x ->
\y ->

# Force the interpreter to infer a row-polymorphic record type for `x`
let unify = [ x.a, 1 ]

in  [ (y : forall (p : Fields) . { b : Bool, p }), x ]
