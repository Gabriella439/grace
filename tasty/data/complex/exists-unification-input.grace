# This tests illustrates how higher-rank existential quantification can be used
# to unify disparate types 
[ 1, True ] : List (exists (a : Type) . a)
