# `grace`

Grace (short for [Fall-from-Grace](#name)) is a ready-to-fork implementation of
a JSON-compatible functional programming language with type inference.  You will
most likely be interested in Grace for one of two reasons:

* You need to implement a domain-specific language and you would like to begin
  from a quality existing implementation instead of embedding a syntax tree in
  JSON/YAML

* You're interested in learning more about state-of-the-art algorithms for
  programming language theory by studying a clear and realistic reference
  implementation

If you're interested in code samples, then you can either jump down to the
[Quick tour](#quick-tour) section or check out the
[examples directory](./examples).

You can also try out Fall-from-Grace in your browser by visiting this page:

* [Grace browser](https://trygrace.dev/)

## Build

You can build the `grace` executable using `cabal`:

```bash
$ cabal build exe:grace
```

Note: For older versions of cabal (e.g. version <3), use `cabal new-build exe:grace`.  Known to work for at least cabal v.2.4

You can also build this project using Nix:

```bash
$ nix --extra-experimental-features 'nix-command flakes' build
```

… and you can build the live demo website for this project also using Nix:

```bash
$ nix --extra-experimental-features 'nix-command flakes' build .#website
```

You can also run `grace` without explicitly installing it:

```bash
$ nix --extra-experimental-features 'nix-command flakes' run github:Gabriella439/grace -- --help
```

## Features

Grace implements the following features so that you don't have to:

* Efficient and maintainable parsing

  Grace uses a lexer in conjunction with an Earley parser in order to improve
  the efficiency and predictability of parsing performance.  In particular,
  the parser will run in linear time for any grammar accepted by an LR parser.

* JSON-compatible syntax

  Grace uses the same syntax as JSON for records, lists, and scalar values,
  which means that many JSON expression are already valid Grace expressions:

  ```json
  # This is valid Grace source code
  {
    "clients": [
      {
        "isActive": true,
        "age": 36,
        "name": "Dunlap Hubbard",
        "email": "dunlaphubbard@cedward.com",
        "phone": "+1 (890) 543-2508"
      },
      {
        "isActive": true,
        "age": 24,
        "name": "Kirsten Sellers",
        "email": "kirstensellers@emergent.com",
        "phone": "+1 (831) 564-2190"
      }
    ]
  }
  ```

  Don't like JSON syntax?  No problem, the grammar is easy to change.

* Bidirectional type-inference and type-checking

  Grace's type system is based on the
  [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
  paper.  This algorithm permits most types to be inferred without type
  annotations and the remaining types can be inferred with a single top-level
  type annotation.

* [Dhall](https://dhall-lang.org/)-style imports

  You can import subexpressions by referencing their path or URL.  You can also
  import JSON in the way same way since Grace is a superset of JSON.

* Fast evaluation

  Grace implements [normalization by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation)
  to efficiently interpret code.

  The interpreter also doesn't need to warm up and has a low startup overhead of
  tens of milliseconds, so Grace is suitable for short-lived command-line
  tools.

* Fixes to several JSON design mistakes

  The Grace interpreter supports comments, leading/trailing commas, and unquoted
  field names for input code while still emitting valid JSON output.

  This means that you can use Grace as a starting point for an  ergonomic JSON
  preprocessor (similar to [jsonnet](https://jsonnet.org/), but with types).

* Error messages with source locations

  Grace generates accurate and informative source locations in error messages,
  such as this:

  ```
  Not a subtype

  The following type:

    Bool

  (input):1:18: 
    │
  1 │ [ { x: 1 }, { x: true } ]
    │                  ↑

  … cannot be a subtype of:

    Natural

  (input):1:8: 
    │
  1 │ [ { x: 1 }, { x: true } ]
    │        ↑
  ```

* Syntax highlighting and code formatting

  The interpreter highlights and auto-formats code, both for results and error
  messages.  Note that the code formatter does not preserve comments (in order
  to simplify the implementation).

* Open records and open unions

  Grace extends the bidirectional type-checking algorithm with support for
  inferring the types of open records (also known as
  [row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism)) and
  open unions (also known as [polymorphic variants](https://2ality.com/2018/01/polymorphic-variants-reasonml.html)).  This lets you easily work with records or
  unions where not all fields or alternatives are known in advance.

* Universal quantification

  Universal quantification lets you specify "generic" types (i.e. types
  parameterized on other types).

  Universal quantification works with types, open records, and open unions.

Also, the package and the code is extensively commented and documented to help
you get started making changes.  You can also read the
[CONTRIBUTING](./CONTRIBUTING.md) guide for instructions on how to get started.

## Notable omissions

Grace does not support the following language features:

* Input / output ("IO")

  Grace only supports pure computation and doesn't support an effect system for
  managing or sequencing effects

* Type classes

  These require global coherence, which does not play nice with Dhall-style
  path-based imports

* Type synonyms

  You cannot easily create short-hand synonyms for commonly used types

* User-defined datatypes

  All data types in Grace are anonymous (e.g. anonymous records and anonymous
  unions), and there is no concept of a data declaration

* Recursion or recursive data types

  Grace only supports two built-in recursive types, which are `List` and `JSON`,
  but does not support user-defined recursion or anonymous recursion.

* String interpolation

  This is possible, but tricky, to lex, so I decided that it would be simpler
  to remove the feature.

Grace also does not support the following tooling:

* A language server

  I will accept pull requests for this, but I don't plan on maintaining a
  language server myself since it's a lot of work and is a large surface area
  to maintain.

* Code formatter that preserves comments

  I will probably reject pull requests to add this because I expect this would
  really clutter up the implementation and the concrete syntax tree.

* Extensive documentation

  Grace is not really meant to be used directly, but is instead intended to be
  forked and used as a starting point for your own language, so any
  documentation written for Grace would need to be substantially rewritten as
  you adjust the language to your needs.

  That said, this `README` has a brief tour of the language below.

  If you still need an example of a tutorial for a similar language that you can
  adapt, see
  [the Dhall language tour](https://docs.dhall-lang.org/tutorials/Language-Tour.html).

## Development

You can get started on changing the language to your liking by reading the
[CONTRIBUTING](./CONTRIBUTING.md) guide.

If you're interested in upstreaming your changes, then these are the issues and
pull requests I'm most likely to accept:

* Bug fixes

* Improving error messages

* Fixes to build against the latest version of GHC or dependencies

* Adding new built-ins

  … especially if they are likely to be widely used by downstream
  implementations.

* Adding features with a high power-to-weight ratio

  Basically, anything that isn't too complicated and likely to be generally
  useful is fair game, especially if it's easy for forks to delete or disable
  if they don't want it.

* Simpler and clearer ways of implementing existing functionality

  For example, if you think there's a way to simplify the type-checker,
  parser, or evaluator without too much regression in functionality then I'll
  probably accept it.

* Adding more comments or clearer contributing instructions

  … so that people can more easily adapt the language to their own use case.

* Syntactic sugar

  For example, I'd probably accept pull requests to compress the syntax for
  nested `forall`s or nested lambdas.

These are the issues and pull requests that I'm most likely to reject:

* Anything that significantly increases my maintenance burden

  This project is more of an educational resource, like an executable blog
  post, than a production-ready package.  So I commit to maintaining to this
  about as much as I commit to maintaining a blog post (which is to say: not
  much at all, other than to merge or reject pull requests).

* Anything that significantly deteriorates the clarity of the code

  It's far more important to me that this code is pedagogically useful than the
  code being production-ready.  Again, think of this project as an executable
  tutorial that people can learn from.

* Any request to publish binaries or official releases

  This project is made to be forked, not directly used.  If you want to publish
  anything, then fork the project and maintain binaries/releases yourself.

## Acknowledgments

Your fork doesn't need to credit me or this project, beyond what the
[BSD 3-clause license](./grace-core/LICENSE) requires.  The only thanks I need
is for people to use Grace instead of creating yet another domain-specific
language embedded in JSON or YAML.

## Quick tour

This section provides a lightning tour that covers all language features as
briefly as possible, directed at people who already have some experience with
typed and functional programming languages.

### Command line

This package builds a `grace` executable with the following command-line API:

```bash
$ grace --help
Usage: grace COMMAND
  Command-line utility for the Grace language

Available options:
  -h,--help                Show this help text

Available commands:
  interpret                Interpret a Grace file
  text                     Render a Grace text literal
  format                   Format Grace code
  builtins                 List all built-in functions and their types
  repl                     Enter a REPL for Grace
```

You can use the `interpret` subcommand for interpreting a single file:

```dhall
# ./example.ffg
let greet name = "Hello, " + name + "!"

in  greet "world"
```

```bash
$ grace interpret example.ffg
```
```dhall
"Hello, world!"
```

… and you can specify `-` to process standard input instead of a file, like
this:

```bash
$ grace interpret - <<< '2 + 2'
```
```dhall
4
```

You can also use the `repl` subcommand for interactive usage:

```bash
$ grace repl
```
```dhall
>>> :let x = 1
>>> :let y = 2
>>> x + y
3
```

### Data

Grace supports the following Scalar types:

* `Bool`s, such as `false` and `true`

* `Natural` numbers, such as `0`, `1`, `2`, …

* `Integer`s, such as `-2`, `-1`, `0`, `1`, `2`, …

  `Natural` numbers are a subtype of `Integer`s

* `Real`s, such as `3.14159265`, `6.0221409e+23`, …

  `Integer`s are a subtype of `Real`s

* `Text`, such as `""`, `"Hello!"`, `"ABC"`, …

  `Text` supports JSON-style escape sequences

… and the following complex data structures:

* `List`s, such as `[]`, `[ 2, 3, 5 ]`, …

* `Optional` types, such as `null`

  There is no special syntax for a present `Optional` value.  Every type `T` is
  a subtype of `Optional T`.  For example:

  ```dhall
  [ 1, null ] : List (Optional Natural)
  ```

* Records, such as `{}`, `{ x: 2.9, y: -1.4 }`

  Record field names usually don't need to be quoted unless they require special
  characters

* Unions, such as `Left 1`, `Right True`

  Any identifer beginning with an uppercase character is a union tag.  You don't
  need to specify the type of the union, since union types are open and
  inferred.

* JSON, such as `[ 1, [ true, "" ] ]`

  … which is a supertype of all expressions that are also valid JSON.

Note that unions are the only data structure that is not JSON-compatible,
since JSON does not support unions.

You can nest complex data structures arbitrarily, such as this example list of
package dependencies:

```dhall
[ GitHub
    { repository: "https://github.com/Gabriel439/Haskell-Turtle-Library.git"
    , revision: "ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
    }
, Local { path: "~/proj/optparse-applicative" }
, Local { path: "~/proj/discrimination" }
, Hackage { package: "lens", version: "4.15.4" }
, GitHub
    { repository: "https://github.com/haskell/text.git"
    , revision: "ccbfabedea1cf5b38ff19f37549feaf01225e537"
    }
, Local { path: "~/proj/servant-swagger" }
, Hackage { package: "aeson", version: "1.2.3.0" }
]
```

### Types and annotations

You can annotate a value with type using the `:` operator.  The left argument
to the operator is a value and the right argument is the expected type:

```dhall
  true : Bool
# ↑      ↑
# Value  Expected type
```

You can also ask to include the inferred type of an interpreted expression as
a type annotation using the `--annotate` flag:

```bash
$ grace interpret --annotate - <<< '[ 2, 3, 5 ]'
```
```dhall
[ 2, 3, 5 ] : List Natural
```

Here are some example values annotated with types::

```dhall
true : Bool

"Hello" : Text

1 : Natural

1 : Integer  # `Natural` numbers also type-check as `Integer`s

1 : Real     # All numbers type-check as `Real`s

1 : Optional Natural  # Everything type-checks as `Optional`, too

[ true, false ] : List Bool

[ ] : forall (a : Type) . List a

{ name: "John", age: 24 } : { name: Text, age: Natural }

Left 1 : forall (a : Alternatives) . < Left: Natural | a >

[ Left 1, Right true ]
  : forall (a : Alternatives) . List < Left: Natural | Right: Bool | a >

Integer/even : Integer -> Bool

[ 1, true ] : JSON  # Any expression that is valid JSON type-checks as `JSON`
```

### Control

Grace supports some operators out-of-the-box, such as:

* Addition: `2 + 3`
* Multiplication: `2 * 3`
* Logical conjunction: `true && false`
* Logical disjunction: `true || false`
* Text concatenation: `"AB" + "CD"`
* List concatenation: `[ 2, 3 ] + [ 5, 7 ]`

You can also consume boolean values using `if` / `then` / `else`
expressions:

```dhall
$ grace interpret - <<< 'if true then 0 else 1'
0
```

You can define immutable and lexically-scoped variables using the `let` and
`in` keywords:

```dhall
let name = "redis"

let version = "6.0.14"

in  name + "-" + version
```

You can access record fields using `.`:

```dhall
let record = { turn: 1, health: 100 }

in  record.turn
```

You can pattern match on a union using the `fold` keyword by providing a
record of handlers (one per alternative):

```dhall
let render
      : < Left: Real | Right: Bool > -> Text
      = fold
          { Left: show
          , Right: \b -> if b then "true" else "false"
          }

in  [ render (Left 2.0), render (Right true) ]
```

Grace supports anonymous functions using `\input -> output` syntax.  For
example:

```dhall
let twice = \x -> [ x, x ]

in  twice 2
```

You can also use the built-in functions, including:

```dhall
# Render any `JSON`-compatible value as `Text`
show : JSON -> Text

# Drop the first N elements from a `List`
List/drop : forall (a : Type) . Natural -> List a -> List a

# Get the first element of a list
List/head
  : forall (a : Type) .
    forall (b : Alternatives) .
      List a -> Optional a

# Annotate each element of a list with its index
List/indexed : forall (a : Type) . List a -> List { index: Natural, value: a }

# Get the last element of a list
List/last
  : forall (a : Type) .
    forall (b : Alternatives) .
      List a -> Optional a

# Compute the length of a list
List/length : forall (a : Type) . List a -> Natural

# Transform each element of a list
map : forall (a : Type) . forall (b : Type) . (a -> b) -> List a -> List b

# Reverse a list
List/reverse : forall (a : Type) . List a -> List a

# Take the first N elements of a list
List/take : forall (a : Type) . Natural -> List a -> List a

# Returns `true` if the `Integer` is even
Integer/even : Integer -> Bool

# Returns `true` if the `Integer` is false
Integer/odd : Integer -> Bool

# Compute the absolute value of an `Integer`
abs : Integer -> Natural
```

For an up-to-date list of builtin functions and their types, run
the `grace builtins` subcommand.

### Type checking and inference

By default, the type-checker will infer a polymorphic type for a function
if you haven't yet used the function:

```bash
$ grace interpret --annotate - <<< '\x -> [ x, x ]'
```
```dhall
(\x -> [ x, x ]) : forall (a : Type) . a -> List a
```

However, if you use the function at least once then the type-checker will
infer a monomorphic type by default, so code like the following:

```dhall
let twice = \x -> [ x, x ]

in  twice (twice 2)
```

… will be rejected with a type error like this:

```
Not a subtype

The following type:

   List Natural

./example.ffg:1:19: 
  │
1 │ let twice = \x -> [ x, x ]
  │                   ↑

… cannot be a subtype of:

   Natural

./example.ffg:1:14: 
  │
1 │ let twice = \x -> [ x, x ]
  │              ↑
```

… because the inner use of `twice` thinks `x` should be a `Natural` and the
outer use of `twice` thinks `x` shoud be a `List Natural`.

However, you can fix this by adding a type signature to make the universal
quantification explicit:

```dhall
let twice : forall (a : Type) . a -> List a         
          = \x -> [ x, x ]

in  twice (twice 2)
```

… and then the example type-checks.  You can read that type as saying that the
`twice` function works `forall` possible `Type`s that we could assign to `a`
(including both `Natural` and `List Natural`)..

You don't need type annotations when the types of values exactly match, but
you do require type annotations to unify types when one type is a proper
subtype of another type.

For example, `Natural` and `Integer` are technically two separate types, so if
you stick both a positive and negative literal in a `List` then type-checking
will fail:

```bash
$ grace interpret - <<< '[ 3, -2 ]'
Not a subtype

The following type:

   Integer

(input):1:7: 
  │
1 │ [ 3, -2 ]
  │       ↑

… cannot be a subtype of:

   Natural

(input):1:3: 
  │
1 │ [ 3, -2 ]
  │   ↑
```

… but if you add an explicit type annotation then type-checking will succeed:

```bash
$ grace interpret - <<< '[ 3, -2 ] : List Integer'
```
```dhall
[ 3, -2 ]
```

### Open records and unions

The interpreter can infer polymorphic types for open records, too.  For
example:

```bash
$ grace interpret --annotate - <<< '\x -> x.foo'
```
```dhall
(\x -> x.foo) : forall (a : Type) . forall (b : Fields) . { foo: a, b } -> a
```

You can read that type as saying that `\x -> x.foo` is a function from a record
with a field named `foo` to the value of that field.  The function type also
indicates that the function works no matter what type of value is present within
the `foo` field and also works no matter what other fields might be present
within the record `x`.

You can also mark fields `Optional` to unify records with mismatched sets of
fields.  For example, the following list won't type-check without a type
annotation because the fields don't match:

```bash
$ grace interpret - <<< '[ { x: 1, y: true }, { x: 2, z: "" } ]'
Record type mismatch

The following record type:

   { z: Text }

(input):1:22: 
  │
1 │ [ { x: 1, y: true }, { x: 2, z: "" } ]
  │                      ↑

… is not a subtype of the following record type:

   { y: Bool }

(input):1:3: 
  │
1 │ [ { x: 1, y: true }, { x: 2, z: "" } ]
  │   ↑

The former record has the following extra fields:

• z

… while the latter record has the following extra fields:

• y
```

… but if we're only interested in the field named `x` then we can use a
type annotation to tell the type-checker that the other fields are `Optional`:

```dhall
[ { x: 1, y: true }, { x: 2, z: "" } ]
    : List { x: Natural, y: Optional Bool, z: Optional Text }
```

The compiler also infers universally quantified types for union alternatives,
too.  For example:

```bash
$ grace interpret --annotate - <<< '[ Left 1, Right true ]'
```
```dhall
[ Left 1, Right true ]
  : forall (a : Alternatives) . List < Left: Natural | Right: Bool | a >
```

The type is universally quantified over the extra union alternatives, meaning
that the union is "open" and we can keep adding new alternatives.  We don't
need to specify the desired type or set of alternatives in advance.

### JSON

You can make all sorts of weird expressions type-check by adding a type
annotation of `JSON`:

```dhall
[ true, 1, [ -2, false, "" ], null, { foo: { } } ] : JSON
```

… but the only way you can consume an expression of type `JSON` is to use they
`fold` keyword, which you can think of as having this type when given a `JSON`
argument:

```dhall
fold
  : forall (a : Type) .
      { array: List a -> a
      , bool: Bool -> a
      , real: Real -> a
      , integer: Integer -> a
      , natural: Natural -> a
      , null: a
      , object: List { key: Text, value: a } -> a
      , string: Text -> a
      } ->
      JSON ->
        a
```

For example, the following expression

```dhall
fold
  { "bool": \b -> if b then 1 else 0
  , "natural": \x -> x
  , "integer": abs
  , "real": \_ -> 1
  , "string": \_ -> 2
  , "null": 3
  , "object": List/length
  , "array": fold { nil: 0, cons: \x -> \y -> x + y : Natural }
  }
  [ true, 1, [ -2, false, "" ], null, { foo: { } } ]
```

… evaluates to `10`.

There is no other way to consume a `JSON` value other than to specify how to
handle every single case, because once you annotate a value as having type
`JSON` then the interpreter can no longer guarantee that the value is a `List`,
record, or a specific scalar value.  This is why you should prefer to use a
more precise type annotation if possible and only use `JSON` as a type
annotation as a last resort.

### Imports

Grace has two ways to import expressions from other sources: Filepath-based
imports and imports using URIs.

#### Imports from files

You can import a Grace subexpression stored within a separate file by
referencing the file's relative or absolute path.

For example, instead of having one large expression like this:

```dhall
[ { name: "Cake donut"
  , batters: [ "Regular", "Chocolate", "Blueberry", "Devil's Food" ]
  , topping: [ "None"
             , "Glazed"
             , "Sugar"
             , "Powdered Sugar"
             , "Chocolate with Sprinkles"
             , "Chocolate"
             , "Maple"
             ]
  }
, { name: "Raised donut"
  , batters: [ "Regular" ]
  , topping: [ "None", "Glazed", "Sugar", "Chocolate", "Maple" ]
  }
, { name: "Old Fashioned donut"
  , batters: [ "Regular", "Chocolate" ]
  , topping: [ "None", "Glazed", "Chocolate", "Maple" ]
  }
]
```

… you can split the expression into smaller files:

```dhall
# ./cake.ffg

{ name: "Cake donut"
, batters: [ "Regular", "Chocolate", "Blueberry", "Devil's Food" ]
, topping: [ "None"
           , "Glazed"
           , "Sugar"
           , "Powdered Sugar"
           , "Chocolate with Sprinkles"
           , "Chocolate"
           , "Maple"
           ]
}
```

```dhall
# ./raised.ffg

{ name: "Raised donut"
, batters: [ "Regular" ]
, topping: [ "None", "Glazed", "Sugar", "Chocolate", "Maple" ]
}
```

```dhall
# ./old-fashioned.ffg

{ name: "Old Fashioned donut"
, batters: [ "Regular", "Chocolate" ]
, topping: [ "None", "Glazed", "Chocolate", "Maple" ]
}
```

… and then reference them within a larger file, like this:

```dhall
[ ./cake.ffg
, ./raised.ffg
, ./old-fashioned.ffg
]
```

You can also import functions in this way, too.  For example:

```dhall
# ./greet.ffg

\name -> "Hello, " + name + "!"
```

```bash
$ grace interpret - <<< './greet.ffg "John"'
```
```dhall
"Hello, John!"
```

Any subexpression can be imported in this way.

#### Imports using URIs

Imports with URIs work similar to the ones using a simple filepath.

Suppose you do not have the `greet.ffg` stored locally but instead it resides
on a web server: `http://example.com/grace/greet.ffg`
You could either download it and reference it by its filepath like demonstrated
in the example above or let the Grace interpreter do the job:

```bash
$ grace interpret - <<< 'http://example.com/grace/greet.ffg "John"'
```
```dhall
"Hello, John!"
```

Grace supports the following URI schemes:

* HTTP: `https://…` or `http://…`

  ```bash
  $ grace interpret - <<< 'https://raw.githubusercontent.com/Gabriel439/grace/5b3c0e11ee4776a42c26c1986bef8a17dd329e2e/prelude/bool/not.ffg true'
  false
  ```

* Files: `file:…`

  ```bash
  $ grace interpret - <<< 'file:/path/to/greet.ffg "John"'
  ```
  ```dhall
  "Hello, John!"
  ```

* Environment variables: `env:…`

  ```bash
  $ MY_VAR='"Hello !"' grace interpret - <<< 'env:MY_VAR'
  ```
  ```dhall
  "Hello !"
  ```

## Prelude

You can import a small standard library of utilities from the following URL:

* [https://raw.githubusercontent.com/Gabriel439/grace/main/prelude/package.ffg](https://raw.githubusercontent.com/Gabriel439/grace/main/prelude/package.ffg)

These utilities provide higher-level functionality that wraps the underlying
builtins.

Here is an example of how to use the Prelude:

```dhall
let prelude =
      https://raw.githubusercontent.com/Gabriel439/grace/main/prelude/package.ffg

in  prelude.bool.not true
```

The Prelude is organized as a large and nested record that you can import.
Each sub-package of the Prelude is a top-level field, and the utilities are
nested fields within each sub-package.

You can also directly import the utility you need, which is faster since it
only requires a single HTTP request:

```dhall
let not =
      https://raw.githubusercontent.com/Gabriel439/grace/main/prelude/bool/not.ffg

in  not true
```

## Name

Like all of my programming language projects, Grace is named after a
character from PlaneScape: Torment, specifically
[Fall-from-Grace](https://torment.fandom.com/wiki/Fall-from-Grace), because
Grace is about
[slaking the intellectual lust](https://torment.fandom.com/wiki/Brothel_for_Slaking_Intellectual_Lusts)
of people interested in programming language theory.

The name of this interpreter conflicts with
[another programming language](http://gracelang.org/applications/), so use the
longer name, "Fall-from-Grace", to disambiguate when it's not clear from the
context.  Either way, you'll want to rename this project when you fork it.
