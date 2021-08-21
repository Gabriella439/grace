# Verify that higher-rank predicative polymorphism is allowed by giving the
# `const` function the highest-rank polymorphic type possible
(\x -> \y -> x) : forall (a : Type) . a -> (forall (b : Type) . b -> a)
