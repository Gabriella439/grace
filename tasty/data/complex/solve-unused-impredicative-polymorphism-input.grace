# This test exercises solving a variable to an impredicative type
\x -> x : List (forall (a : Type) . Bool)
