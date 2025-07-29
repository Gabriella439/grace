# Grace

Grace (short for [Fall-from-Grace](#name)) is a domain-specific programming
language for prompting LLMs.  In particular, Grace is well-suited for building
and auto-generating elaborate prompt chains

You can use Grace in your browser without installing anything by visiting
[trygrace.dev](https://trygrace.dev/).  That website includes an interactive
tutorial and is the recommended way to both learn and get started with using
Grace.

## Features

- Integrated language support for LLMs

  You don't need to install any dependencies or import anything to get started.
  Everything you need is built directly into the language.

  For example, the following self-contained Grace program asks a model to
  generate a list of names and then greets each one:

  ```haskell
  let key = ./openai-key.txt

  let names = prompt{ key }

  let greet{ name } = "Hello, ${name}!"

  in  map greet names
  ```

  … which might output something like:

  ```json
  [ "Hello, Alice!", "Hello, Bob!", "Hello, Charlie!" ]
  ```

- JSON schemas inferred from use

  Notice how the above example doesn't include any prompt for the model.  This
  is because Grace infers the correct JSON schema by working backwards from how
  the model's output is used.

  In particular, the type checker can deduce (without any type annotations)
  that the `names` value is has a Grace type of:

  ```haskell
  List { name: Text }
  ```

  … which you can read as "a `List` of records, each of which has a `name` field
  containing `Text`).

  The interpreter then converts that Grace type into the following matching JSON
  schema to constrain the model's output:

  ```json
  {
    "type": "array",
    "items": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        }
      },
      "required": ["name"],
      "additionalProperties": false
    }
  }
  ```

  Finally, the LLM infers from that JSON schema alone that it should generate a
  JSON-encoded list of names.

- Code generation

  You can ask the LLM to generate a Grace expression of any desired type; even
  functions!

  For example, if we ask the model to generate the following function

  ```haskell
  >>> :let key = ./openai-key.txt

  >>> prompt{ key, code: true, text: "Greet every name" } : List { name : Text } -> List Text
  ```

  … then the model generates code equivalent to what we wrote:

  ```
  \xs -> map (\{ name } -> "Hello, ${name}!") xs
  ```

  … which means that we could have written our original example as:

  ```haskell
  let key = ./openai-key.txt

  let names = prompt{ key }

  let process{ names: List{ name: Text } } : List Text =
          prompt{ key, code: true, text: "Greet every name" } names

  in  process{ names }
  ```

  … which will generate something similar to this intermediate program after
  prompting:

  ```haskell
  let names = [ { name: "Alice" }, { name: "Bob" }, { name: "Charlie" } ]

  let process{ names: List{ name: Text } } : List Text =
          (\xs -> map (\{ name } -> "Hello, ${name}!") xs) names

  in  process{ names }
  ```

  … and that simplifies to:

  ```json
  [ "Hello, Alice!", "Hello, Bob!", "Hello, Charlie!" ]
  ```

- Type inference

  Grace is a typed programming language, meaning that many programming errors
  are caught ahead of time before the program is run.  However, Grace programs
  often require no type annotations because the intermediate types can be
  inferred from use.

  Moreover, Grace's LLM support is *type-aware*, meaning that the inferred
  types *inform* and *constrain* the model's outputs.

### Command line

This Haskell package builds a `grace` executable with the following command-line
API:

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

```haskell
# ./greet.ffg

let key = ./openai-key.txt

let names = prompt{ key }

let greet{ name } = "Hello, ${name}!"

in  map greet names
```

```bash
$ grace interpret --openAIKey "$(< openai-key.txt)" ./greet.ffg
```
```json
[ "Hello, Alice!", "Hello, Bob!", "Hello, Charlie!" ]
```

… and you can specify `-` to process standard input instead of a file, like
this:

```bash
$ grace interpret - <<< '2 + 2'
```
```haskell
4
```

You can also use the `repl` subcommand for interactive usage:

```bash
$ grace repl
```
```haskell
>>> :let key = ./openai-key.txt
>>> prompt{ key } : List { "First Name": Text, "Last Name": Text }
[ { "First Name": "John", "Last Name": "Doe" }
, { "First Name": "Jane", "Last Name": "Smith" }
, { "First Name": "Alice", "Last Name": "Johnson" }
, { "First Name": "Michael", "Last Name": "Brown" }
, { "First Name": "Emily", "Last Name": "Davis" }
]
```

## Name

Like all of my programming language projects, Grace is named after a character
from PlaneScape: Torment, specifically
[Fall-from-Grace](https://torment.fandom.com/wiki/Fall-from-Grace).

The name of this programming language conflicts with
[another programming language](http://gracelang.org/applications/), so use the
longer name, "Fall-from-Grace", to disambiguate when it's not clear from the
context.
