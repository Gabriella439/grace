# This verifies that two record types cannot unify if they share the same
# fields variable but different monomorphic fields
let f : forall (a : Fields) . { a } -> { x : Bool, a }
      = \x -> x

in  f
