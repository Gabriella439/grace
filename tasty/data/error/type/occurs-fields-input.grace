# This test exercises the occurs check for fields variables
\f ->
\r ->
\s ->
  [ (f : forall (a : Fields) . { a } -> { x : Bool, a }) r
  , f (f r)
  ]
