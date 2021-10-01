# This ensures that `null`'s type is inferred as `Optional a?` where `a?` is an
# unsolved type variable, instead of `forall (a : Type) . Optional a`, since
# the latter type would lead to a type error due to relying on impredicativity.
\x -> [ null, x ]
