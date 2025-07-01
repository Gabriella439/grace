# Contributing to Grace

This document explains how to edit, build, and run this project, both if you
want to change your fork of the language or if you want to upstream improvements
to the original language.

The easiest way to build the project is using `cabal`, and the most commonly
used commands are:

```bash
$ cabal build exe:grace                    # Build the `grace` executable
$ cabal test                               # Run all tests
$ cabal test tasty                         # Faster: run only tasty tests
$ cabal test tasty --test-option=--accept  # Update golden tests
$ cabal test doctest                       # Run only doctests
$ cabal haddock --hyperlink-source         # Generate Haskell documentation
```

You can also enable coverage checking by running this step before running the
tests:

```bash
$ cabal configure --enable-coverage
```

You'll probably also want to use [`ghcid`](https://github.com/ndmitchell/ghcid)
or
[`haskell-language-server`](https://github.com/haskell/haskell-language-server)
for interactive development.

This project also provides `devShells` for Nix users, but it's not necessary
for project development.

The project tries to be as maintainable possible, meaning that most mistakes
you make will be caught by the type-checker and you will only need to update a
few places to make the most common changes.  However, there are a few places
that you need to remember to update and the type-checker won't remind you.

For example, any time you add a new language feature you will need to update
the parser in order to actually use the feature, and nothing will automatically
remind you to do that.

Generally speaking, if you're not sure where to begin then start by identifying
the most closely-related language feature and searching the codebase for all
occurrences of the matching constructor in the [`Syntax`][syntax] tree.

## GHCJS instructions

If you want to make changes to the website you will need to build using GHCJS,
which entails the following commands:

```bash
$ nix develop .#ghcjs
[nix-shell]$ cabal v1-configure --ghcjs --disable-tests
[nix-shell]$ cabal v1-build
```

… and if you want to test the website, then run the following additional
command after each build:

```bash
[nix-shell]$ cp dist/build/try-grace/try-grace.jsexe/all.js website/js/all.min.js
```

… and then open [`./website/index.html`](./website/index.html) in your
browser.

The test suite will not work and also `ghcid` will not work when developing
using GHCJS.

## Adding a new built-in function

To add a new built-in function, edit the [`Syntax`][syntax] module to add a new
constructor to the `Builtin` type, then fix all of the type errors, which should
consist of:

* Specifying the type of the builtin
* Specifying how to pretty-print the builtin

Then, edit the [`Normalize`][normalize] module to change the `apply` function to
add a case for handling the newly-added builtin.

Finally, add support for parsing the built-in by:

* Adding a new `Token` for the `Builtin` in the [`Lexer`][lexer] module
* Adding the built-in name to the `reserved` words in the [`Lexer`][lexer]
  module
* Adding a new parsing rule in the [`Parser`][parser] module.

## Adding a new operator

Adding a new operator is basically the same as adding a new built-in, with
the main change being that you change the `Operator` type within the
[`Syntax`][syntax] module, instead of changing the `Builtin` type.

The other difference is that you will change the [`Normalize`][normalize] in a
different place (where all of the operator logic is).

## Adding a new scalar literal and the corresponding scalar type

To add a new scalar type, edit the [`Syntax`][syntax] module to add a new
constructor to the `Scalar` type (representing the scalar literal).  Also, edit
the [`Monotype`][monotype] module to add a new constructor to the `Scalar` type
in that module (representing the corresponding scalar type).  Then fix all of
the type errors, which will consist of:

* Specifying how to prettyprint the scalar literal
* Specifying how to prettyprint the scalar type
* Specifying how to infer the type of the scalar literal

  … by returning the matching scalar type you just created

Finally, edit the [`Lexer`][lexer] and [`Parser`][parser] modules to lex and
parse the new scalar literal and scalar type you just created.  

## Adding a new keyword

Probably the easiest way to add a new keyword is to study how an existing
keyword is implemented, such as the `if` / `then` / `else` keyword.  Search
the codebase for all occurrences of the `If` constructor and follow the pattern.

## Adding a new complex type

Just like keywords, the easiest way to add a new complex type is to study how an
existing complex type is implemented, such as the `List` type.  Search the
codebase for all occurrences of the `List` constructor and follow the pattern.

## Pretty-printing

When adding new keywords or complex type you will need to take care to remember
to update the pretty-printing logic.  By default, the code will go into an
infinite loop if you forget to do this, and this post explains the reason why:

* [Pretty-print syntax trees with this one simple trick](https://www.haskellforall.com/2020/11/pretty-print-syntax-trees-with-this-one.html)

You don't need to worry about this if you are adding new built-ins / operators /
scalars, since those are already handled uniformly.

## Bidirectional type-checking

This is probably the hardest part of making any changes, especially changes that
add new keywords or complex types, since they cannot be handled uniformly.

If you're new to logical notation in general, then I recommend first reading
[A tutorial implementation of a dependently typed lambda calculus](http://www.cs.ru.nl/~wouters/Publications/Tutorial.pdf),
which explains the correspondence between logical notation and Haskell code.

Then read the
[Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](https://arxiv.org/abs/1306.6032)
paper which explains the general principles behind the type-checking algorithm.
However, there are a few nuances that are not obvious from a casual reading of
the paper, so follow up by reading the [`Infer`][infer] module, which is heavily
commented with things I had to figure out in the course of attempting to
implement the paper.

[lexer]: ./src/Grace/Lexer.hs
[infer]: ./src/Grace/Infer.hs
[monotype]: ./src/Grace/Monotype.hs
[normalize]: ./src/Grace/Normalize.hs
[parser]: ./src/Grace/Parser.hs
[syntax]: ./src/Grace/Syntax.hs
