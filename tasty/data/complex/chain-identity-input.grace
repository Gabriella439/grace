# This test verifies that the type-checker doesn't blow up even when
# instantiating `id` to a very high-order type
let id : forall (a : Type) . a -> a = \x -> x

in  id id id id id id id id id id id id id id id id id id id id id id id true
