# This test illustrates that you can still consume values of mixed types so long
# as the mixed types are unified with a typed hole and the consuming function is
# universally quantified over the hole
let input0
      : List ?
      = [ 2, true ]

let function0
      : forall (a : Type) . List a -> Natural
      = List/length

let input1
      : List { x: Natural, ? }
      =  [ { x: 1, y: true }, { x: 2, z: "" } ]

let handler
      : forall (a : Fields) . { x : Natural, a } -> Natural
      = \record -> record.x

let function1
      : forall (a : Fields) . List { x : Natural, a } -> List Natural
      = List/map handler

in  { example0: function0 input0
    , example1: function1 input1
    }
