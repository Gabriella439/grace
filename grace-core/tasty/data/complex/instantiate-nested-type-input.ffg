# This test is designed to exercise as many code paths as possible for the
# `Grace.Type.substitute{Type,Fields,Alternatives}` family of functions
let f : forall (a : Type)
      . forall (a : Fields)
      . forall (a : Type)
      . forall (t : Type)
      . forall (r : Fields)
      . forall (u : Alternatives)
      .  a
      -> t
      -> { r }
      -> < u >
      -> List < C : { y : a, t : Text, b : Bool, n : Natural, t : t, r : { r }, u : < u > } >
      = \x ->
        \t ->
        \r ->
        \u ->
        [ C { y: x, t: "", b: false, n: 0, t: t, r: r, u: u } ]

let g : forall (a : Fields)
      . forall (a : Alternatives)
      . forall (a : Fields)
      . forall (t : Type)
      . forall (r : Fields)
      . forall (u : Alternatives)
      .  { a }
      -> t
      -> { r }
      -> < u >
      -> List < C : { y : { a }, t : Text, b : Bool, n : Natural, t : t, r : { r }, u : < u > } >
      = f

let h : forall (a : Alternatives)
      . forall (a : Type)
      . forall (a : Alternatives)
      . forall (t : Type)
      . forall (r : Fields)
      . forall (u : Alternatives)
      .  < a >
      -> t
      -> { r }
      -> < u >
      -> List < C : { y : < a >, t : Text, b : Bool, n : Natural, t : t, r : { r }, u : < u > } >
      = f

let example0 = f 1

let example1 = g { foo: 1 }

let example2 = h (Foo 1)

in  { }
