# This tests that Church-encoding a list works correctly
let nil : forall a . forall list . (a -> list -> list) -> list -> list
        = \cons -> \nil -> nil

let cons
        : forall a
        .   a
        ->  (forall list . (a -> list -> list) -> list -> list)
        ->  (forall list . (a -> list -> list) -> list -> list)
        = \x -> \list_ -> \cons -> \nil -> cons x (list_ cons nil)

let and : (forall list . (Bool -> list -> list) -> list -> list) -> Bool
       = \list -> list (\x -> \y -> x && y) True

in  and (cons True (cons False (cons True nil)))
