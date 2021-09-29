# Grace encoding of the proof that if you assume that axiom of choice then you
# can prove double-negation
#
# This is taken from:
#
# https://www.haskellforall.com/2017/02/the-curry-howard-correspondence-between.html
(   \noMiddle
->  \doubleNegation
->  merge
      { Left: \x -> x
      , Right: \x -> merge {} (doubleNegation x)
      }
      noMiddle
) :     (forall (b : Type) . < Left : b | Right : b -> <> >)
    ->  (forall (a : Type) . ((a -> <>) -> <>) -> a)
