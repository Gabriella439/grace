# This test illustrates a type variable being instantiated to a polymorphic
# type
let f : forall (a : Type) . a -> a
      = \x -> x

let g : forall (c : Type) . (forall (b : Type) . b -> b) -> (c -> c)
      = f

in  { }
