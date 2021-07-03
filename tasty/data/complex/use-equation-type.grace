forall (a : Fields) .
forall (b : Alternatives) .
forall (c : Type) .
  { x: Bool, a } ->
  { x: Bool, a } ->
  < L: Natural | b > ->
  c ->
    { field1: List { x: Bool, a }, field2: List < L: Natural | b > }
