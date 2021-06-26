# This test exercises impredicative polymorphism that is unused, which is the
# only case where impredicative polymorphism is permitted.
[ true ] : List (forall (a : Type) . Bool)
