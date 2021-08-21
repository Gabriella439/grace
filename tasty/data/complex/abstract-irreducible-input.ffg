# The purpose of this test is to exercise the interpretation of "stuck"
# expressions that would otherwise evaluate if their arguments were not abstract
{ or: \x -> \y -> x || y
, and: \x -> \y -> x && y
, apply: \f -> \x -> f x
, field: \x -> x.foo
, _if: \x -> \y -> \z -> if x then y else z
, times: \x -> \y -> x * y
, plus: \x -> \y -> x + y
, append: \x -> \y -> x ++ y
, fold: \x -> Natural/fold x
}
