# The purpose of this test is to exercise weird code paths for the well-formed
# type check
{ field0 = \r -> (r : forall (a : Fields) . forall (b : Type) . { | a })
, field1 = \u -> (u : forall (a : Alternatives) . forall (b : Type) . < | a >)
}
