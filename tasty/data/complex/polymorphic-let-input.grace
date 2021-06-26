# This verifies that a type with a polymorphic type annotation can be
# instantiated to multiple types within the same expression
let id : forall (a : Type) . a -> a = \x -> x

in  { example0: id true
    , example1: id { }
    }
