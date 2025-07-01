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
identifier  = (LOWER / "_") *(ALPHANUM / "_" / "-" / "/")

; A name for one of the alternatives of a union
alternative = (UPPER / "_") *(ALPHANUM / "_" / "-" / "/")

lambda = "\" 1*name-binding "->" expression

; If you annotate a function argument with a type you have to parenthesize the
; annotation.
;
; BAD:
;
; ```
; \x : Natural ->
;     let f y : Natural : Natural = x + y
;     in  f 2
; ```
;
; GOOD:
;
; ```
; \(x : Natural) ->
;     let f (y : Natural) : Natural = x + y
;     in  f 2
; ```
name-binding
  ; plain bound variable: `\x -> …`
  = identifier

  ; bound variable with a type annotation: `\(x : T) -> …`
  / "(" identifier ":" type ")"

  ; destructure a record: `\{ x, y : Natural } -> …`
  / "{" [ identifier [ ":" type ] *("," identifier [ ":" type ]) ] "}"

; Note: Every sequence of `let`s (even top-level `let`s) must have a matching
; `in`.  Dangling `let`s are a parse error in any context.
;
; BAD:
;
;     let x = 2
;     let y = 3  # Missing `in` at the end, which is a syntax error
;
; GOOD:
;
;     let x = 2
;     let y = 3
;     in  { x, y }  # To "export" let bindings, package them in a record
let = 1*binding "in" expression

; Every binding must begin with a `let` because Grace is not
; whitespace-sensitive.
;
; BAD:
;
; ```
; let x = 2
;     y = 3  # Missing `let`, so this is misparsed as `let x = 2 y = 3`
; in  x + y
; ```
;
; GOOD:
;
; ```
; let x = 2
; let y = 3
; in  x + y
; ```
binding = "let" identifier *name-binding [ ":" type ] "=" expression

if = "if" expression "then" expression "else" expression

annotation = application *( operator application ) ":" type

; Operators in descending order of precedence
operator
    = "/"
    / "%"
    / "*"
    / "-"
    / "+"
    / ">="
    / ">"
    / "<="
    / "<"
    / "!="
    / "=="
    / "&&"
    / "||"

application
  ; Keyword to prompt an LLM to generate a plain value (the default) or Grace
  ; code (when given `code: true` as an argument)
  ;
  ; The `prompt` keyword technically only takes one argument.  Arguments after
  ; the first one are treated as ordinary function applications (which comes in
  ; handy if you're generating Grace code for a function)
  = "prompt" 1*projection

  ; Keyword to pattern match on a union
  ;
  ; Just like the `prompt` keyword, the `fold` keyword
  ; only takes one argument and subsequent arguments are
  ; ordinary function applications
  / "fold" 1*projection

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
; solely exists so that you can reference shadowed-variables.
variable = identifier [ "@" integer ]

boolean = "true" / "false"

number = natural / integer / real

; Positive integers are parsed as `Natural` numbers
natural = 1*DIGIT

; Negative integers are parsed as `Integer`s
integer = "-" natural

; All other numbers are parsed as `Real`s
real = [ "-" ] 1*DIGIT "." 1*DIGIT

string = '"' *( character / interpolation / escape) '"'

character = %x20-21 / %x23-5B / %x5D-7E

; Interpolated expressions must have type `Text`.  Grace does *not* perform
; any automatic conversion of interpolated values to `Text`.  If you want to
; interpolate a number, then use:
;
; ```
; "… ${show number} …"
; ```
interpolation = "${" expression "}"

; NOTE: You can escape a string interpolation using a backslash like this:
;
; ```
; "Example: \${…}"
; ```
;
; … if you don't want Grace to interpret the string interpolation.  This comes
; in handy if you, say, want to use Grace to generate a Bash script without
; interpreting Bash string interpolations.
escape = "\\" ( %x22 / "\\" / "n" / "t" / "r" / "b" / "f" / "$")

list = "[" [ expression *( "," expression ) ] "]"

record = "{" [ projection-value *( "," projection-value ) ] "}"

projection-value
  ; Grace uses JSON syntax for projection values: ':' instead of '='.  This is
  ; one way in which Grace differs from its predecessor (Dhall) because Grace
  ; is intended to be JSON-compatible
  = field ":" expression

  ; Field punning.  In other words, `{ x }` is the same thing as `{ x: x }`
  / field

field = identifier / alternative / string

builtin
    = "show "           ; JSON -> Text
    / "List/drop"       ; forall (a : Type) . Natural -> List a -> List a
    / "List/head"       ; forall (a : Type) . List a -> Optional a
    / "List/indexed"    ; forall (a : Type) . List a -> List { index: Natural, value: a }
    / "List/last"       ; forall (a : Type) . List a -> Optional a
    / "List/length"     ; forall (a : Type) . List a -> Natural
    / "map"             ; forall (a : Type) (b : Type) . (a -> b) -> List a -> List b
    / "List/take"       ; forall (a : Type) . Natural -> List a -> List a
    / "Integer/even"    ; Integer -> Bool
    / "Integer/odd"     ; Integer -> Bool
    / "abs"             ; Integer -> Natural

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
