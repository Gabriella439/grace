# This test verifies that trailing and leading separators are supported

{ example0: [ , 1, 2, 3, ]
, example1: { , x: 1, y: True, }
, example2: \x -> x : forall (a : Fields) . { , x : Integer, y: Bool, a,  }
, example3: \x -> x : < | A : Natural | B : Bool | >
, example3:
    \x -> x : forall (a : Alternatives) . < | A : Natural | B : Bool | a | >
}
