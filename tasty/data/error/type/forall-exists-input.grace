# This test illustrates that existentially quantified field variables (like `a`)
# will not unify with other variables outside of the scope of the existential
# quantification.
let values
      : List (exists (a : Fields) . { x: Natural, a })
      =  [ { x: 1, y: true }, { x: 2, z: "" } ]

let handler
      : forall (a : Fields) . { x : Natural, a } -> Natural
      = \record -> record.x

in  List/map handler values
