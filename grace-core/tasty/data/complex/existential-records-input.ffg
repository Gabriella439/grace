# This illustrates how you can use existential quantification to ignore fields
# that you're not interested when trying to unify record types
[ { x: 1, y: True }
, { x: 1, z: "" }
] : List (exists (a : Fields) . { x : Natural, a })
