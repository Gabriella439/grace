# This verifies that the idiom of unifying present/absent fields using
# `Optional` types works
[ { x: Some 1, y: None {} }, { x: None {}, y: Some true } ]
