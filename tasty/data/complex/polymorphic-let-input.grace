# This verifies that a type with a polymorphic type annotation can be
# instantiated to multiple types within the same expression
let id : forall (a : Type) . a -> a = \x -> x

in  { x = id True, y = id { } }
