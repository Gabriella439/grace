Here's the grammar for Grace if you're not exactly sure what the language does
and does not permit so that you don't run into trivial syntax errors:

```
expression
  = lambda
  / let
  / if
  / annotation
  / operator

; A variable name (e.g. `x`)
identifier  = (LOWER / "_") (ALPHANUM / "_" / "-" / "/")

; An name for one of the alternatives of a union
alternative = (UPPER / "_") (ALPHANUM / "_" / "-" / "/")

lambda = "\" 1*name-binding "->" expression

name-binding = identifier / "(" identifier ":" type ")"

; Note: Every sequence of `let`s (even top-level `let`s) must have a matching
; `in`.  Dangling `let`s are a parse error in any context
let = 1*binding "in" expression

binding = "let" identifier *name-binding [ ":" type ] "=" expression

if = "if" expression "then" expression "else" expression

annotation = operator ":" type

; Grace has an *extremely* limited set of operators.  There are no other
; operators
operator = and *( "||" and )
and = plus *( "&&" plus )
plus = times *( "+" times )
times = application *( "*" application )

application
  ; Keyword to prompt an LLM to generate either JSON (the default) or Grace
  ; code (when given `code: true` as an argument)
  ;
  ; The `prompt` keyword technically only takes one argument.  Arguments after
  ; the first one are treated as ordinary function applications (which comes in
  ; handy if you're Grace code for a function)
  = "prompt" 1*projection

  ; Keyword to pattern match on a union
  ;
  ; Just like the `prompt` keyword, the `merge` keyword
  ; only takes one argument and subsequent arguments are
  ; ordinary function applications
  / "merge" 1*projection

  ; Ordinary function application
  / 1*projection

projection = primitive *( "." field )

primitive
  = variable
  / "null"
  / boolean
  / number
  / string
  / builtin
  / list
  / record

  ; Note that all alternatives need an argument.  By itself, an alternative is
  ; a function expecting one argument (the alternative's contents).  If an
  ; alternative is empty you still need to store an empty record inside of it.
  / alternative

  ; An absolute, relative, or home-anchored URI
  / file

  ; A URL
  / url

  / "(" expression ")"

; The optional integer at the end is a "Namespaced De Bruijn index".  See:
;
; https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html
;
; The vast majority of the time you do *not* need to use this feature and it
; mainly exists so that when Grace β-reduces something like this:
;
;     \x -> (\y x -> y) x
;
; … the result can be pretty-printed without variable name mangling like this:
;
;     \x x -> x@1
;
; … although it *is* legal in input Grace expressions if you *really* want to
; refer to shadowed variables.
variable = identifier [ "@" integer ]

boolean = "true" / "false"

number = natural / integer / real

; Positive integers are parsed as `Natural` numbers
natural = 1*DIGIT

; Negative integers are parsed as `Integer`s
integer = "-" natural

; All other numbers are parsed as `Real`s
real = [ "-" ] 1*DIGIT "." 1*DIGIT

string = '"' *( character / escape ) '"'
character = %x20-21 / %x23-5B / %x5D-7E
escape = "\\" ( %x22 / "\\" / "n" / "t" / "r" / "b" / "f" )

record = "{" [ projection-value *( "," projection-value ) ] "}"

list = "[" [ expression *( "," expression ) ] "]"

projection-value
  ; Grace uses JSON syntax for projection values: ':' instead of '='.  This is
  ; one way in which Grace differs from its predecessor (Dhall) because Grace
  ; is intended to be JSON-compatible
  = field ":" expression

  ; Field punning.  In other words, `{ x }` is the same thing as `{ x: x }`
  / field

builtin
    = "Real/equal"      ; Real -> Real -> Bool
    / "Real/lessThan"   ; Real -> Real -> Bool
    / "Real/negate"     ; Real -> Real
    / "Real/show"       ; Real -> Text
    / "List/drop"       ; forall (a : Type) . Natural -> List a -> List a
    / "List/equal"      ; forall (a : Type) . (a -> a -> Bool) -> List a -> List a -> Bool
    / "List/fold"       ; forall (a : Type) (b : Type) . { cons: a -> b -> b, nil: b } -> List a -> b
    / "List/head"       ; forall (a : Type) . List a -> Optional a
    / "List/indexed"    ; forall (a : Type) . List a -> List { index: Natural, value: a }
    / "List/last"       ; forall (a : Type) . List a -> Optional a
    / "List/length"     ; forall (a : Type) . List a -> Natural
    / "List/map"        ; forall (a : Type) (b : Type) . (a -> b) -> List a -> List b
    / "List/reverse"    ; forall (a : Type) . List a -> List a
    / "List/take"       ; forall (a : Type) . Natural -> List a -> List a
    / "Integer/even"    ; Integer -> Bool
    / "Integer/negate"  ; Integer -> Integer
    / "Integer/odd"     ; Integer -> Bool
    / "Integer/abs"     ; Integer -> Natural
    / "JSON/fold        ; forall (a : Type) .
                        ;   { array: List a -> a
                        ;   , bool: Bool -> a
                        ;   , real: Real -> a
                        ;   , integer: Integer -> a
                        ;   , natural: Natural -> a
                        ;   , "null": a
                        ;   , object: List { key: Text, value: a } -> a
                        ;   , string: Text -> a
                        ;   } ->
                        ;   JSON ->
                        ;     a
    / "Natural/fold"    ; forall (a : Type) . Natural -> (a -> a) -> a -> a
    / "Text/equal"      ; Text -> Text -> Bool

field = identifier / alternative / string

type = quantified-type

quantified-type = *forall function-type

forall = "forall" 1*( "(" identifier ":" domain ")" ) "." 
domain = "Type" / "Fields" / "Alternatives"

function-type = application-type *( "->" application-type )

application-type
  = "List" primitive-type
  / "Optional" primitive-type
  / primitive-type

primitive-type
  = identifier  ; Type variable
  / "Bool"
  / "Real"
  / "Integer"
  / "Natural"
  / "Text"
  / "JSON"
  / record-type
  / union-type
  / "(" type ")"

record-type = "{" [ field ":" type *( "," field ":" type ) ] [ "," identifier ] "}"
union-type = "<" [ alternative ":" type *( "|" alternative ":" type ) ] [ "|" identifier ] ">"
```
