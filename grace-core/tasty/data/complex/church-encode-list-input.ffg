# This tests that Boehm-Berarducci-encoding a list works correctly
let nil : forall (a : Type)
        . forall (list : Type)
        . (a -> list -> list) -> list -> list
        = \cons -> \nil -> nil

let cons
        :   forall (a : Type)
        .   a
        ->  (forall (list : Type) . (a -> list -> list) -> list -> list)
        ->  (forall (list : Type) . (a -> list -> list) -> list -> list)
        = \x -> \list -> \cons -> \nil -> cons x (list cons nil)

let and :   (forall (list : Type) . (Bool -> list -> list) -> list -> list)
        ->  Bool
        = \list -> list (\x -> \y -> x && y) true

in  and (cons true (cons false (cons true nil)))
