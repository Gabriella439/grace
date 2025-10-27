# Grace

Grace (short for [Fall-from-Grace](#name)) is a domain-specific programming
language for prompting models.  In particular, Grace is well-suited for building
and auto-generating elaborate prompt chains

You can use Grace in your browser without installing anything by visiting
[trygrace.dev](https://trygrace.dev/).  That website includes an interactive
tutorial and is the recommended way to both learn and get started with using
Grace.

## Features

- Integrated language support for models

  You don't need to install any dependencies or import anything to get started.
  Everything you need is built directly into the language.

  The language provides a built-in `prompt` function for prompting a model:

  ```haskell
  >>> let key = ./openai.key : Key

  >>> prompt{ key, text: "Generate a list of names" }
  "
  Here are 40 varied first names (mixed genders and cultures):

  - Aiden
  - Sofia
  - Mateo
  …
  - Mabel
  - Imani
  - Zane

  Want names filtered by gender, culture, style (modern/vintage), or as full names/surnames?"
  ```

  … and you can structure the output by giving a type annotation:

  ```haskell
  >>> prompt{ key, text: "Generate a list of names" } : List Text
  [ "Ava Thompson"
  , "Liam Patel"
  , "Sophia Martinez"
  …
  , "Jackson Rivera"
  , "Zoe Wilson"
  , "Aiden Park"
  ]
  ```

  If the type is sufficiently self-explanatory, you can even omit the prompt:

  ```haskell
  >>> prompt{ key } : List { name: Text }
  [ { "name": "Alice" }
  , { "name": "Bob" }
  , { "name": "Charlie" }
  , { "name": "Diana" }
  , { "name": "Evan" }
  ]
  ```

  In fact, you can omit the type, too, if the type can be inferred from use:

  ```haskell
  >>> for { name } of prompt{ key } in "Hello, ${name}!"
  [ "Hello, Alice!"
  , "Hello, Bob!"
  , "Hello, Carol!"
  , "Hello, Dave!"
  , "Hello, Eve!"
  ]
  ```

- JSON schemas inferred from use

  That last example works even without a prompt, schema, or type because Grace's
  type checker reasons backwards from how the output is used to infer the
  correct JSON schem, like this:

  - type checker infers that the `name` variable must be `Text`

    … because the `name` variable is interpolated into "Hello, ${name}!"

  - the type checker infers that the `prompt` function must generate a `List`

    … because the program loops over the output using a `for … of` loop.

  - the type checker infers each element of the `List` has type `{ name: Text }`

    … because the `for … of` loop destructures each element using `{ name }`

  - therefore the `prompt` function outputs a value of type `List{ name: Text }`

  … which you can read as "a `List` of records, each of which has a `name` field
  containing `Text`".

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

  Finally, the model infers from that JSON schema alone (without any additional
  prompt) that it should generate a JSON-encoded list of names.

- Code generation

  You can prefix the `prompt` keyword with `import` to ask the model to generate
  a Grace expression of any type.  For example:

  ```haskell
  >>> import prompt{ key, text: "increment" }
  \n -> n + 1
  ```

  You can use an explicit type annotation to guide the generated code:

  ```haskell
  >>> import prompt{ key, text: "increment" } : { input: Natural } -> { output: Natural }
  \{ input } -> { "output": input + 1 }
  ```

  … and if the type is informative enough then you can omit the prompt:

  ```haskell
  >>> import prompt{ key } : { "Job Description": Text } -> { "Is Finance?": Bool }
  let key = 🔒

  in  \{ "Job Description" } ->
        prompt
          { "key":
              key
          , "text":
              "
              Determine whether the following job description is for a finance role.
              Return a JSON object with a single boolean field \"Is Finance?\": true if it is a finance role, otherwise false.
              Answer only valid JSON, nothing else.
  
              Job description:
              ${.'Job Description'}
              "
          , "model":
              null
          , "search":
              null
          , "effort":
              null
          }
          : { "Is Finance?": Bool }
  ```

  Notice in that last example how the model can generate code which itself
  `prompt`s an model.  Neat!

  Inferred types also guide the code generation process, too!

  ```haskell
  >>> let upper = import prompt{ key, text: "uppercase" } in "Hello, ${upper "gabby"}!"
  "Hello, GABBY!"
  ```

  There the model infers that the type of the `upper` function needs to be
  `Text -> Text` (a function whose input is `Text` and whose output is `Text`)
  and generates an function matching that type which uppercases `Text`.

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

for { name } of prompt{ key: ./openai-key.txt }

in  "Hello, ${name}!"
```

```bash
$ grace interpret ./greet.ffg
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
>>> let key = ./openai-key.txt
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
