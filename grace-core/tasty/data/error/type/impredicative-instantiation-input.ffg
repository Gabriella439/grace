# This test verifies that impredicative instantiation of polymorphic types is
# not permitted
let f : forall (a : Type) . a -> a
      = \x -> x

let g : (forall (b : Type) . b -> b) -> (forall (c : Type) . c -> c)
      = f

in  g
