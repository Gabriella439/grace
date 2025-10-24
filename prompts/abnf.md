Here's the grammar for Grace if you're not exactly sure what the language does
and does not permit so that you don't run into trivial syntax errors:

```
expression
  = lambda
  / let
  / if
  / annotation  ; Everything else (operators, projections, literals, etc.)

identifier
  ; Naked variable names begin with a lowercase letter or "_"
  ;
  ; Examples: `x`, `name`, `toLicense`
  = (LOWER / "_") *(ALPHANUM / "_" / "-" / "/")

  ; Quoted variable names begin with `.` and are surrounded with single quotes
  ;
  ; Examples: `.'Rationale'`, `.'Section Header'`, `.'Website - Backend'`
  / "." single-quoted

lambda = "\" 1*binding "->" expression

; A bound variable with an optional type annotation and optional default value
name-binding = identifier [ ":" type ] [ "=" expression ]

; One or more names bound as arguments for a lambda or named functions
binding
  ; Plain bound variable:
  ;
  ;     \x -> …
  ;
  ;     let f x = … in …
  ;
  ;     let greet .'Name' = "Hi, ${.'Name'}!" in greet "Alice"
  ;
  = identifier

  ; Bound variable with an optional type annotation and optional default value:
  ;
  ;     \(x : T = v) -> …
  ;
  ;     let f (x : T = v) = … in …
  ;
  ;     let greet (.'Name' : Text = "Alice") = "Hi, ${.'Name'}!" in greet null
  ;
  ; You can have just the type annotation:
  ;
  ;     \(x : T) -> …
  ;
  ;     let f (x : T) = … in …
  ;
  ; … or just the default value:
  ;
  ;     \(x = v) -> …
  ;
  ;     let f (x = v) = … in …
  ;
  ;     let greet (.'Name' : Text) = "Hi, ${.'Name'}!" in greet "Alice"
  ;
  ; You can even omit both and just parenthesize the bound variable, although
  ; this is not idiomatic since you'd usually omit the parentheses in that case:
  ;
  ;     \(x) -> …
  ;
  ;     let f (x) = … in …
  ;
  ;     let greet (.'Name') = "Hi, ${.'Name'}!" in greet "Alice"
  ;
  / "(" name-binding ")"

  ; Destructure a record function argument:
  ;
  ;     \{ a, b } -> …
  ;
  ;     let f{ a, b } = … in …
  ;
  ;     let greet{ "Name" } = "Hi, ${.'Name'}!" in greet{ "Name": "Alice" }
  ;
  ;     let greet{ .'Name' } = "Hi, ${.'Name'}!" in greet{ "Name": "Alice" }
  ;
  ; Record fields destructured in this way can have optional type annotations
  ; and optional default values:
  ;
  ;     \{ a, b : T0, c = v0, d : T1 = v1 } -> …
  ;
  ;     let f { a, b : T0, c = v0, d : T1 = v1 } = … in …
  ;
  ;     let greet{ "Name" : Text = "Alice" } = "Hi, ${.'Name'}!" in greet{ }
  ;
  ;     let greet{ .'Name' : Text = "Alice" } = "Hi, ${.'Name'}!" in greet{ }
  / "{" [ name-binding *( "," name-binding ) ] "}"

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
let = 1*assignment "in" expression

; Every assignment must begin with a `let` because Grace is not
; whitespace-sensitive.
;
; BAD:
;
;     let x = 2
;         y = 3  # Missing `let`, so this is misparsed as `let x = 2 y = 3`
;     in  x + y
;
; GOOD:
;
;     let x = 2
;     let y = 3
;     in  x + y
assignment =
    ; Define a simple value:
    ;
    ;     let x = 2 in x + 1
    ;
    ; … or a function of one or more arguments:
    ;
    ;     let increment x = x + 1 in increment 2
    ;
    ; Function definitions can destructure their arguments and this is the most
    ; idiomatic way to define functions in Grace:
    ;
    ;     let greet{ name } = "Hi, ${name}!" in greet{ name: "Alice" }
    = ("let" identifier *binding [ ":" type ] "=" expression)

    ; Destructure the right-hand side:
    ;
    ;     let { x, y } = { x: 1, y: 2 } in x + y
    / ("let" binding "=" expression)

    ; For comprehension (like a list comprehension, but works on both `List`s
    ; and `Optional`s)
    ;
    ;     for x of [ 1, 2 ]
    ;     for y of [ false, true ]
    ;     in  { x, y }
    ;
    ;     = [ {x: 1, y: false }
    ;       , {x: 1, y: true  }
    ;       , {x: 2, y: false }
    ;       , {x: 2, y: true  }
    ;       ]
    / ("for" binding "of" expression)

if = "if" expression "then" expression "else" expression

; Optional type annotation:
;
;     e : T
annotation = application *( operator application ) [ ":" type ]

; Operators in descending order of precedence
;
; This is the same precedence order as C operators
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
  ; Keyword to prompt an LLM to generate a JSON value (the default) or Grace
  ; code (when preceded with `import`)
  = [ "import" ] "prompt" projection

  ; Keyword to make an HTTP request to fetch a JSON value (the default) or Grace
  ; code (when preceded with `import`)
  = [ "import" ] "http" projection

  ; Keyword to convert text to a JSON value (the default) or Grace code (when
  ; preceded with `import`)
  = [ "import" ] "read" projection

  ; Keyword to fetch a JSON file from GitHub (the default) or Grace code (when
  ; preceded with `import`)
  = [ "import" ] "github" projection

  ; Keyword to pattern match on a union
  / "fold" projection

  ; Ordinary function application (left-associative)
  / application projection

  / projection

; Optionally project a field (from a record) or an element (from a list)
projection = primitive *smaller

smaller
  ; You can access a record field using `record.field`.
  = "." field

  ; You can project multiple fields from a record
  / "." "{" [ field *( "," field ) ] "}"

  ; You can also index into a list using dot notation (e.g. `list.index`)  Just
  ; like Python, you can index from the end of the list using negative numbers
  ; (e.g. `list.-1` to get the last element of the list).
  / "." integer

  ; You can slice into a list using `xs[m:n]` just like in Python.  Slice
  ; indices may also be negative and both indices are optional.
  / "[" [ integer ] ":" [ integer ] "]"

primitive
  = identifier  ; bound variable (e.g. `x`)
  / "null"
  / boolean
  / number
  / string
  / builtin
  / list
  / record

  ; NOTE: all alternatives need an argument.  If an alternative is empty you
  ; still need to store an empty record inside of it (e.g. `Foo{ }`)
  / alternative primitive

  ; An absolute path (beginning with `/`) or relative path (beginning with `../`
  ; or `./`)
  / file

  ; A URI (supported schemes: `https` / `http` / `env` / `file`)
  / uri

  / "(" expression ")"

boolean = "true" / "false"

number = natural / integer / real

; Positive integers are parsed as `Natural` numbers
natural = 1*DIGIT

; Signed integers are parsed as `Integer`s
integer = ("+" / "-") natural

; All other numbers are parsed as `Real`s
real = [ ( "+" / "-" ) ] 1*DIGIT "." 1*DIGIT

; Strings support two modes:
;
; - Single-line string literals, like:
;
;       "abc"
;
; - Multi-line string literals, like:
;
;       "
;       Line 1
;
;       Line 3
;       "
string = single-line-string / multi-line-string

; Single-line string literals only support escaped newlines (i.e. `\n`)
single-line-string =
  %x22 *( single-line-character / interpolation / single-line-escape) %x22

; A character other than " or \
single-line-character = %x20-21 / %x23-5B / %x5D-10FFFF

; NOTE: You can escape a string interpolation using a backslash like this:
;
; ```
; "Example: \${…}"
; ```
;
; … if you don't want Grace to interpret the string interpolation.  This comes
; in handy if you, say, want to use Grace to generate a Bash script without
; interpreting Bash string interpolations.
single-line-escape =
  "\\" ( %x22 / "\\" / "/" / "n" / "t" / "r" / "b" / "f" / "$" / ("u" 4HEXDIG) )

; These string literals can span multiple lines and leading indentation is
; stripped.  For example, this:
;
;     let example =
;             "
;             Line 1
;
;             Line 3
;             "
;
; … is the same thing as:
;
;     let example = "Line 1\n\nLine 3\n"
;
; The difference between a single-line string literal and a multi-line string
; literal is that in a multi-line string literal the `"` is followed by a
; newline (which must be present and is stripped).  For example, this:
;
;     let example =
;             "
;             Line 1"
;
; … is the same thing as:
;
;     let example = "Line 1"
multi-line-string =
  %x22 %x0A *( multi-line-character / interpolation / multi-line-escape) %x22

; A character other than " or \
;
; Literal tabs and newlines are also permitted, unlike single-line strings.
multi-line-character = %x09-0A / %x20-21 / %x23-5B / %x5D-10FFFF

; NOTE: You cannot escape newlines or tabs in a multi-line string literal
; (because you can and should use an actual newline or tab character instead of
; an escaped one).
multi-line-escape =
  "\\" ( %x22 / "\\" / "/" / "r" / "b" / "f" / "$" / ("u" 4HEXDIG) )

; Interpolated expressions must have type `Text`.  Grace does *not* perform
; any automatic conversion of interpolated values to `Text`.  If you want to
; interpolate a number, then use:
;
; ```
; "… ${show number} …"
; ```
;
; Interpolated expressions do not need to be escaped:
;
; BAD:
;
;     \input -> "Hello, ${input.\"First Name\"}!"
;
; GOOD:
;
;     \input -> "Hello, ${input."First Name"}!"
interpolation = "${" expression "}"

; A name for one of the alternatives of a union
alternative
  ; Unquoted alternative names begin with an uppercase letter
  = UPPER *(ALPHANUM / "_" / "-" / "/")

  ; Quoted alternative names are surrounded with single quotes
  / single-quoted

single-quoted = "'" (single-quoted-character / single-quoted-escape) "'"

; A character other than ' or \
single-quoted-character = %x20-26 / %x28-5B / %x5D-10FFFF

; Similar to the rule for "escape" except replacing " with ' and also not
; including an escape sequence for $ (since it's not necessary because a quoted
; alternative name can't include an interpolation).
single-quoted-escape =
  "\\" ( "'" / "\\" / "/" / "n" / "t" / "r" / "b" / "f" / ("u" 4HEXDIG) )

; Lists allow optional leading/trailing commas.
list = "[" [ "," ] [ expression *( "," expression ) ] [ "," ] "]"

; Records allow optional leading/trailing commas
record = "{" [ "," ] [ projection-value *( "," projection-value ) ] [ "," ] "}"

projection-value
  ; Grace uses JSON syntax for projection values: ':' (not '=')
  = field ":" expression

  ; Field punning.  In other words, `{ x }` is the same thing as `{ x: x }`
  / field

field
  = identifier

  ; Field names can be alternative names, too.  This is necessary so that you
  ; can `fold` unions (since the field names need to match the union's
  ; alternative names)
  / alternative

  ; You can quote field names, too, which comes in handy if a field has
  ; characters that would otherwise be forbidden (e.g. spaces or punctuation)
  ; (e.g. `record."Example field"` or `{ "Example field": true }`)
  / string

builtin
    = "show"     ; JSON -> Text  ; Renders argument as JSON
    / "yaml"     ; JSON -> Text  ; Renders argument as YAML
    / "indexed"  ; forall (a : Type) . List a -> List { index: Natural, value: a }
    / "length"   ; forall (a : Type) . List a -> Natural
    / "map"      ; forall (a : Type) (b : Type) . (a -> b) -> List a -> List b
    / "abs"      ; Integer -> Natural
    / "reveal"   ; Key -> Text
    / "some"     ; forall (a : Type) . a -> Optional a

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
  / "Key"
  / record-type
  / union-type
  / "(" type ")"

; Records types allow optional leading/trailing commas
record-type =
    "{"
    [ "," ]
    [ field ":" type *( "," field ":" type ) ]
    [ "," identifier ]  ; Fields variable (e.g. `{ x: Text, other }`
    [ "," ]
    "}"

; Union types allow optional leading/trailing bars
union-type =
    "<"
    [ "|" ]
    [ alternative ":" type *( "|" alternative ":" type ) ]
    [ "|" identifier ]  ; Alternatives variable (e.g. `< Left: Natural | other >`)
    [ "|" ]
    ">"
```
