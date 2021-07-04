# Show that we can existentially quantify the fields we don't care about and
# process such records using universally quantified functions:
let values
      : List (exists (a : Fields) . { x: Natural, a })
      =  [ { x: 1, y: true }, { x: 2, z: "" } ]

let handler
      : forall (a : Fields) . { x : Natural, a } -> Natural
      = \record -> record.x

in  List/map handler values
