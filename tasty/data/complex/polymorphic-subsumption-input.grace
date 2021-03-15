# This checks that a polymorphic type is a subtype of a corresponding
# instantiated monomorphic type
((\x -> x) : forall a . a -> a) : Bool -> Bool
