# This verifies that multiple occurrences of the same constructor (e.g. `C` in
# this example) are not necessarily treated as belonging to the same union type
\x -> C (C x)
