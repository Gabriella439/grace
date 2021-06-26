# This test exercises weird code paths for the `equate*` unification utilities
\a ->
\r ->

\b ->
\u ->

let # This forces `r`'s type to be instantiated to a record with an unsolved
    # fields variable
    field0 = [ true, r.x ]

in  { # This triggers instantiation of `a`'s unsolved type to a record type with
      # an unsolved fields variable
      field1: [ r, a ]

      # This triggers instantiation of `b`'s unsolved type to a union type with
      # an unsolved alternatives variable
    , field2: [ L 1, b ]
    }
