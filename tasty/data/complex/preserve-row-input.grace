# This verifies that type inference correctly detects that the input and
# output record should have the same fields variable
\x -> let y = x.a && x.b in x
