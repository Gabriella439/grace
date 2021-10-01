# This test illustrates a type variable being instantiated a more specific
# type without impredicative polymorphism
let f : forall (a : Type) . a -> a
      = \x -> x

# Carefully note the `forall (c : Type)` has to be floated out for this to work.
# If the type annotation were:
#
# (forall (b : Type) . b -> b) -> (forall (c : Type) . c -> c)
#
# … then that would require impredicative polymorphism
let g : forall (c : Type) . (forall (b : Type) . b -> b) -> (c -> c)
      = f

in  { }
