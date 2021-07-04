# `grace`

Grace is a ready-to-fork implementation of a functional programming language
with type inference.  You will most likely be interested in Grace for one of
two reasons:

* You need to implement a domain-specific language and you would like begin from
  a quality existing implementation instead of starting from scratch

* You're interested in learning more about state-of-the-art algorithms for
  programming language theory by studying a clear and realistic reference
  implementation

## Features

Grace implements the following features so that you don't have to:

* Fast and maintainable parsing

  Grace uses a high-performance lexer in conjunction with an LR parsing package
  in order to guarantee efficient and predictable parsing performance.  This
  means that you can easily extend or amend Grace's grammar without taking
  special precautions to avoid performance pitfalls.

* JSON-compatible syntax

  Grace uses the same syntax as JSON for records, lists, scalar values, which
  means that many JSON expression are already valid Grace expressions:

  ```dhall
  # This is valid Grace source code
  {
    "clients": [
      {
        "isActive": true,
        "age": 36,
        "name": "Dunlap Hubbard",
        "email": "dunlaphubbard@cedward.com",
        "phone": "+1 (890) 543-2508",
      },
      {
        "isActive": true,
        "age": 24,
        "name": "Kirsten Sellers",
        "email": "kirstensellers@emergent.com",
        "phone": "+1 (831) 564-2190",
      }
    ]
  }
  ```

  Don't like JSON syntax?  No problem, the grammar is easy to change.

* Type inference using bidirectional type-inference

  Grace's type system is based on the
  [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://www.cl.cam.ac.uk/~nk480/bidir.pdf)
  paper.  This algorithm permits most types to be inferred without type
  annotation and the remaining types to be inferred with a single top-level
  type annotation.

* JSON-compatible type system

  JSON permits all sorts of nonsense that would normally be rejected by typed
  languages, but Grace's type system is sufficiently advanced that most JSON
  expressions can be made valid with a type signature, like this:

  ```dhall
  [ 1, true ] : List (exists (a : Type) . a)
  ```

* [Dhall](https://dhall-lang.org/)-style file-path imports

  You can import Grace code by referencing its relative or absolute path.  You
  can also import JSON in the way same way since Grace is a superset of JSON.

  For example, you can import JSON with a type annotation so that you don't
  need to amend the original JSON:

  ```dhall
  ./input.json : List (exists (a : Type) . a)
  ```

* Fast evaluation

  Grace implements [normalization by evaluation](https://en.wikipedia.org/wiki/Normalisation_by_evaluation)
  to efficiently interpret code.  Combined with parsing and type-checking
  optimizations this means that the interpreter will tear through any code you
  throw at it.

  The interpreter also doesn't need to warm up and has a low startup overhead of
  tens of milliseconds.

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
  [row polymorphisms](https://en.wikipedia.org/wiki/Row_polymorphism)) and
  open unions (also known as [polymorphic variants](https://2ality.com/2018/01/polymorphic-variants-reasonml.html)).  This lets you easily work with records or
  sums where not all fields or alternatives are known in advance.

* Universal quantification and existential quantification

  Universal quantification lets you specify "generic" types (i.e. types
  parameterized on other types).

  Existential quantification lets you specify incomplete / partial types
  (i.e. types with holes that that the interpreter infers).

  Both universal and existential quantification work with types, open records,
  and open unions.

Also, the package and the code is extensively commented and documented to help
you get started making changes.

## Name

Like all of my programming language projects, Grace is named after a
character from PlaneScape: Torment, specifically
[Fall-from-Grace](https://torment.fandom.com/wiki/Fall-from-Grace), because
Grace is about
[slaking the intellectual lust](https://torment.fandom.com/wiki/Brothel_for_Slaking_Intellectual_Lusts)
of people interested in programming language theory.
