# This tests that Natural/fold doesn't have a gross inefficiency by implementing
# the even function in terms of Natural/fold
let not : Bool -> Bool = \b -> if b then False else True

in let even : Natural -> Bool = \n -> Natural/fold n not True

in  even 1000000
