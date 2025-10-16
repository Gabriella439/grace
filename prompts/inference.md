At [work](https://mercury.com/jobs) I've been researching how to improve the ergonomics of prompt chaining and I wanted to share and open source some of what I've done.  This initial post is about how I've been experimenting with using *bidirectional type inference* to streamline *prompt chaining*.

"Prompt chaining" is a prompt engineering technique that splits a larger task/prompt into multiple smaller tasks/prompts which are chained together using code.  For example, instead of prompting a model to generate a poem in one prompt like this:

> Write a poem based off this idea:
> 
> `${idea}`
> 
> … by following this process:
> 
> - First think through the form, stanza count, lines per stanza, and rhyme scheme
> - Then choose a poetic style (tone, voice, and literary devices) based on the poem's form
> - Then write a complete poem based on that plan

… you can split it into smaller prompts, like this:

##### `structure` prompt:

> Plan the structure of a new poem based on this idea
> 
> `${idea}`
> 
> Describe its form, stanza count, lines per stanza, and rhyme scheme

##### `style` prompt:

> Given this poem structure:
> 
> - Form: `${structure."Form"}`
> - Stanzas: `${structure."Stanza Count"}`
> - Lines per stanza: `${structure."Lines per Stanza"}`
> - Rhyme scheme: `${structure."Rhyme Scheme"}`
> 
> Choose a poetic style: tone, voice, and literary devices to emphasize

##### `poem` prompt:

> Write a complete poem based on this idea:
> 
> `${idea}`
> 
> Structure:
> - Form: `${structure."Form"}`
> - Stanzas: `${structure."Stanza Count"}`
> - Lines per stanza: `${structure."Lines per Stanza"}`
> - Rhyme scheme: `${structure."Rhyme Scheme"}`
> 
> Style:
> - Tone: `${style."Tone"}`
> - Voice: `${style."Voice"}`
> - Literary Devices: `${style."Literary Devices"}`

Why might you want to do this?

- to improve the quality of the results
  
  Models perform better when working on more constrained subproblems.  Splitting a larger prompt into smaller prompts helps the model stay focused at each step.

- to introspect intermediate results
  
  This comes in handy when you want to log, validate, or correct intermediate results.

- to perform actions in between prompts
  
  You might want to take the output of one prompt, use that to call some tool, then use the output of that tool to decide what the next prompt should be, which you can't do with a single prompt.

In other words, prompt chaining unlocks greater *accuracy*, *control*, and *flexibility* for prompt engineering.

#### The problem

The main issue with prompt chaining is that it is a huge pain in the ass; if you start do anything a little bit complicated you need to start using structured outputs (i.e. JSON), which adds a whole lot of boilerplate to the process:

- you have to define the schema for each intermediate step of the process
  
  You typically do this by defining your data model in your host programming language (e.g. a Pydantic model in Python) or directly defining your JSON schema

- You have to instruct the model to produce JSON and explain the shape of the expected output

- (Depending on the framework) you have to decode the JSON into your data model

For small prompt chaining pipelines this isn't too hard, but it starts to get annoying to define all these schemas when you scale this up to more sophisticated prompt chaining pipelines.

So as a thought experiment I wanted to create a research prototype that handled all of that for you so that you didn't need to specify any schemas at all.  In other words I wanted to build a programming language that harnessed *bidirectional type inference* to perform *schema inference* for prompts with structured JSON outputs.

#### Example

I'll cut to the case by showing the above prompt chain written as a program in this language:

```haskell
\{ key } ->

let concatSep = import github
      { owner: "Gabriella439"
      , repository: "grace"
      , path: "prelude/text/concatSep.ffg"
      }

let generatePoem idea =
        let structure = prompt
                { key
                , text: "
                    Plan the structure of a new poem based on this idea:

                    ${idea}

                    Describe its form, stanza count, lines per stanza, and rhyme scheme.
                    "
                }

        let renderedStructure = "
                - Form: ${structure."Form"}
                - Stanzas: ${show (structure."Stanza Count": Natural)}
                - Lines per stanza: ${show (structure."Lines per Stanza" : Natural)}
                - Rhyme scheme: ${structure."Rhyme Scheme"}
                "

        let style = prompt
                { key
                , text: "
                    Given this poem structure:

                    ${renderedStructure}

                    Choose a poetic style: tone, voice, and literary devices to emphasize.
                    "
                }

        let renderedStyle = "
                - Tone: ${style."Tone"}
                - Voice: ${style."Voice"}
                - Literary Devices: ${concatSep ", " style."Literary Devices"}
                "

        let poem = prompt
                { key
                , text: "
                    Write a complete poem based on this idea:

                    ${idea}

                    Structure:

                    ${renderedStructure}

                    Style:

                    ${renderedStyle}
                    "
                }

        in { structure, style, poem }

in  generatePoem "The stillness of early morning"
```

The above prompt chain is a real program in an real (albeit research) [programming language named Fall-from-Grace](https://github.com/Gabriella439/grace) (or "Grace" for short).  Specifically, this is implemented in a [branch of the language](https://github.com/Gabriella439/grace/tree/gabriella/llm) that adds a magic `prompt` keyword that works like in the above example.

If you run the above example, you might get an output like this:

```json
{
  "structure": {
    "Form": "Free verse with a sprinkle of rhyme",
    "Lines per Stanza": 4,
    "Rhyme Scheme": "ABCB",
    "Stanza Count": 5
  },
  "style": {
    "Literary Devices": [
      "imagery",
      "alliteration",
      "personification",
      "enjambment"
    ],
    "Tone": "nostalgic",
    "Voice": "reflective"
  },
  "poem": "### The Stillness of Early Morning\n\n**I**  \nIn the gentle rise of dawn's embrace,  \nA whisper of winds through sleepy skies,  \nThe world lies suspended in an echo  \nOf yesterday's dreams, reluctantly saying goodbye.\n\n**II**  \nSoft orange light graces fields of dew,  \nCradling tenacious tendrils of the past.  \nMorning breathes upon the waiting world  \nWith promises that this serenity will last.\n\n**III**  \nWaves of silence surge across the meadow,  \nNature's symphony gently cradles rest.  \nA chorus of leaves sings its soft refrain,  \nComforted by morning's tender caress.\n\n**IV**  \nThe silence dips to caress weary trees,  \nWhose branches waltz to whispers soft and slow.  \nEach tendril holds close the morning's intention,  \nBefore the day's demands, the chaos, start to show.\n\n**V**  \nEarly rays dance upon a tranquil sea,  \nA canvas painted through a dreamy haze.  \nTime sits by, savoring the gentle hush,  \nAs the world awakens, stirs, and finally plays."
}
```

… and if you're curious what the formatted poem looks like:

### The Stillness of Early Morning

**I**  
In the gentle rise of dawn's embrace,  
A whisper of winds through sleepy skies,  
The world lies suspended in an echo  
Of yesterday's dreams, reluctantly saying goodbye.

**II**  
Soft orange light graces fields of dew,  
Cradling tenacious tendrils of the past.  
Morning breathes upon the waiting world  
With promises that this serenity will last.

**III**  
Waves of silence surge across the meadow,  
Nature's symphony gently cradles rest.  
A chorus of leaves sings its soft refrain,  
Comforted by morning's tender caress.

**IV**  
The silence dips to caress weary trees,  
Whose branches waltz to whispers soft and slow.  
Each tendril holds close the morning's intention,  
Before the day's demands, the chaos, start to show.

**V**  
Early rays dance upon a tranquil sea,  
A canvas painted through a dreamy haze.  
Time sits by, savoring the gentle hush,  
As the world awakens, stirs, and finally plays.

#### Type inference

The sample Grace program hardly specifies any types (mainly the final expected type for the `poem`: `Text`).  The reason this works is because Grace supports *bidirectional type inference*, which means that Grace can work backwards from how intermediate results are used to infer their schemas.

I'll illustrate this with a contrived Grace example:

```haskell
\{ key } ->

let numbers = prompt{ key, text: "Give me two numbers" }

in  { x: numbers.x
    , y: numbers.y
    , sum: numbers.x + numbers.y : Integer
    }
```

… which might produce an output like this:

```bash
$ grace interpret ./numbers.ffg
```
```json
{ "x": 7, "y": 14, "sum": 21 }
```

When Grace analyzes this program the type checker works backwards from this expression:

```haskell
numbers.x + numbers.y : Integer
```

… and reasons about it like this:

- the addition produces an `Integer`, therefore `numbers.x` and `numbers.y` must also be `Integer`s

- therefore `numbers` is a record with two fields, `x` and `y`, both of which are `Integer`s
  
  … or using Grace syntax, the inferred type of `numbers` is: `{ x: Integer, y: Integer }`[^1]

- therefore the output of the `prompt` command must have the same type

… and then Grace generates a JSON schema for the prompt which looks like this:

```json
{ "type": "object",
  "properties": {
    "x": { "type": "integer" },
    "y": { "type": "integer" }
  },
  "required": [ "x", "y" ],
  "additionalProperties": false
}
```

Of course, you *can* specify types if you want (and they're more lightweight than schemas in traditional prompt chaining frameworks).  For example:

```bash
$ grace repl
>>> prompt{ key: ./openai.key : Key, text: "Give me a first and last name" } : { first: Text, last: Text }
{ "first": "Emily", "last": "Johnson" }
>>> prompt{ key: ./openai.key : Key, text: "Give me a list of names" } : List Text
[ "Alice"
, "Bob"
, "Charlie"
, "Diana"
, "Ethan"
, "Fiona"
, "George"
, "Hannah"
, "Isaac"
, "Jack"
]
```

However in our original example we don't need to specify intermediate types because when the type-checker sees this code:

```haskell
let structure = prompt
        { key
        , text: "
            Plan the structure of a new poem based on this idea:

            ${idea}

            Describe its form, stanza count, lines per stanza, and rhyme scheme.
            "
        }

let renderedStructure = "
        - Form: ${structure."Form"}
        - Stanzas: ${show (structure."Stanza Count" : Natural)}
        - Lines per stanza: ${show (structure."Lines per Stanza" : Natural)}
        - Rhyme scheme: ${structure."Rhyme Scheme"}
        "
```

… the compiler can reason backwards from how the `structure` value is used to infer that the JSON schema for the `prompt` needs to be:

```json
{ "type": "object",
  "properties": {
    "Form": { "type": "string" },
    "Stanza Count": { "type": "integer" },
    "Lines per Stanza": { "type": "integer" },
    "Rhyme Scheme": { "type": "string" }
  },
  "required": [
    "Form",
    "Stanza Count",
    "Lines per Stanza",
    "Rhyme Scheme"
    ],
  "additionalProperties": false
}
```

#### Tool use

Grace also supports generating *sum types* (a.k.a. tagged unions), and you can imagine using this to subsume traditional tool use frameworks.

For example, consider this Grace program:

```haskell
\{ key } ->

let concatSep = import github
      { owner: "Gabriella439"
      , repository: "grace"
      , path: "prelude/text/concatSep.ffg"
      }

let call = fold
      { HttpRequest: \x -> "curl " + x.url
      , ShellCommand: \x -> concatSep " " ([ x.executable ] + x.arguments)
      }

in  map call (prompt{ key, text: "Call some tools" })
```

This doesn't actually *run* any tools (I haven't added any callable tools to my work-in-progress branch yet), but just renders the tool use as a string for now:

```bash
$ grace interpret ./tools.ffg
```
```json
[ "curl https://api.example.com/data", "ls -l -a" ]
```

However, the idea is that you can model a tool as a sum type with one constructor per callable tool, and in the above example the type checker infers that the sum type representing one tool call is:

```haskell
< HttpRequest: { url: Text }
| ShellCommand: { executable: Text, arguments: List Text }
>
```

… so the inferred type of `call` is:

```haskell
call : < HttpRequest: …, ShellCommand: … > -> Text
```

… but since we `map` the `call` function over the output of the `prompt` the type checker infers that the `prompt` needs to generate a `List` of tool calls:

```haskell
prompt{ key, text: "Call some tools" } : List < HttpRequest: …, ShellCommand: … >
```

… and then Grace does some magic under the hood to convert that type to the equivalent JSON schema.

What's particularly neat about this example is that the prompt is so incredibly bare ("Call some tools") because all the information the model needs is present in the schema.

#### Schema-driven prompting

We can explore this idea of using the schema to drive the prompt instead of prose using an example like this:

```haskell
prompt{ key: ./openai.key : Key, text: "Generate some characters for a story" }
  : List
    { "The character's name": Text
    , "The most memorable thing about the character": Text
    , "The character's personal arc": Text
    }
```
```json
[ { "The character's name": "Aveline Thatcher"
  , "The character's personal arc":
      "Aveline starts as a skeptical journalist who doubts the stories of mythical creatures. Over time, she becomes a firm believer, risking her career to uncover the truth and protect these creatures."
  , "The most memorable thing about the character":
      "The intricate tattoo of a phoenix on her forearm that seems to glow when she discovers hidden truths."
  }
, { "The character's name": "Kelan Frost"
  , "The character's personal arc":
      "A former rogue alchemist who turns hero after he inadvertently creates a dangerous substance. Driven by guilt, Kelan seeks redemption by finding an antidote and saving his village."
  , "The most memorable thing about the character":
      "His iridescent blue eyes that seem to see into one's soul, a side effect of his alchemical experiments."
  }
, { "The character's name": "Luciana Blair"
  , "The character's personal arc":
      "Luciana is a reclusive artist who initially fears the world outside her home. After a mysterious vision rejuvenates her, she sets out on a journey of self-discovery, ultimately finding both her voice and courage."
  , "The most memorable thing about the character":
      "Her ability to paint scenes before they happen, which she attributes to the visions she sees in her dreams."
  }
, { "The character's name": "Ezra Hartman"
  , "The character's personal arc":
      "Once a charismatic but self-centered lawyer, Ezra is confronted with a moral crisis that forces him to reevaluate his values. He chooses a path of integrity, becoming an advocate for justice."
  , "The most memorable thing about the character":
      "His perfectly tailored suits that slowly become more casual, symbolizing his transformation and shifting priorities."
  }
, { "The character's name": "Seraphine Mora"
  , "The character's personal arc":
      "Seraphine is a young music prodigy who loses her hearing after an accident. Battling despair, she learns to embrace a new way of 'hearing' music through vibrations and her other senses."
  , "The most memorable thing about the character":
      "The ethereal way she 'dances' with the music, using her entire body to express each note's emotion."
  }
]
```

Grace is a superset of JSON and since JSON supports arbitrary field names so does Grace!  Field names in Grace support arbitrary capitalization, punctuation, and whitespace as long as you quote them, and we can use the field names to "smuggle" the description of each field into the schema.

#### Conclusion

Hopefully this gives you some idea of why I've begun to think of prompt chaining as a programming languages problem.  Type inference is just the beginning and I think it is possible to use a domain-specific programming language not just to simplify the code but to ultimately unlock greater reasoning power.

I'm going to continue to use Grace as a research vehicle for prompt chaining but my LLM-enabled [branch of Grace](https://github.com/Gabriella439/grace/tree/gabriella/llm) (like Grace itself) is not really intended to be used in production and I created it mainly as a proof-of-concept for where I'd like prompt chaining frameworks to go.  If I do end up eventually productionizing this research I will create a proper fork with its own name and the whole works.

- [ ] [^1]: To be completely pedantic the inferred Grace type for  `numbers` is `forall (other : Fields) . { x: Integer, y: Integer, other }` which essentially means "a record with at least two fields, `x` and `y`, but it might have other fields which we don't care about".
