-- | This module implements support for file-based imports
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
      -- * Resolve
    , resolve
    ) where

import Data.Text (Text)
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Grace.Value (Value)
import System.FilePath ((</>))

import qualified Data.Text.IO    as Text.IO
import qualified Grace.Infer     as Infer
import qualified Grace.Normalize as Normalize
import qualified Grace.Parser    as Parser
import qualified Grace.Syntax    as Syntax
import qualified System.Exit     as Exit
import qualified System.FilePath as FilePath
import qualified System.IO       as IO

{-| Input to the `interpret` function

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code Text
    -- ^ Source code

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret :: Input -> IO (Type, Value)
interpret  input = do
    text <- case input of
        Path file -> Text.IO.readFile file
        Code text -> return text

    let name = case input of
            Path file -> file
            Code _    -> "(input)"

    expression <- case Parser.parse name text of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message
            Exit.exitFailure
        Right expression -> do
            return expression

    let here = case input of
            Path file -> file
            Code _    -> "./."

    resolvedExpression <- resolve here expression

    case Infer.typeOf resolvedExpression of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message
            Exit.exitFailure
        Right inferred -> do
            return (inferred, Normalize.evaluate [] resolvedExpression)

{-| Resolve all imports, replacing each import with its inferred type and normal
    form
-}
resolve :: FilePath -> Syntax FilePath -> IO (Syntax (Type, Value))
resolve _ (Syntax.Variable name index) = do
    return (Syntax.Variable name index)

resolve here (Syntax.Lambda name body₀) = do
    body₁ <- resolve here body₀

    return (Syntax.Lambda name body₁)

resolve here (Syntax.Application function₀ argument₀) = do
    function₁ <- resolve here function₀

    argument₁ <- resolve here argument₀

    return (Syntax.Application function₁ argument₁)

resolve here (Syntax.Annotation annotated₀ annotation) = do
    annotated₁ <- resolve here annotated₀

    return (Syntax.Annotation annotated₁ annotation)

resolve here (Syntax.Let bindings₀ body₀) = do
    bindings₁ <- traverse resolveBinding bindings₀

    body₁ <- resolve here body₀

    return (Syntax.Let bindings₁ body₁)
  where
    resolveBinding (Syntax.Binding name maybeType assignment₀) = do
        assignment₁ <- resolve here assignment₀

        return (Syntax.Binding name maybeType assignment₁)

resolve here (Syntax.List elements₀) = do
    elements₁ <- traverse (resolve here) elements₀

    return (Syntax.List elements₁)

resolve here (Syntax.Record keyValues₀) = do
    keyValues₁ <- traverse resolveKeyValue keyValues₀

    return (Syntax.Record keyValues₁)
  where
    resolveKeyValue (key, value₀) = do
        value₁ <- resolve here value₀

        return (key, value₁)

resolve here (Syntax.Field record₀ key) = do
    record₁ <- resolve here record₀

    return (Syntax.Field record₁ key)

resolve _ (Syntax.Alternative name) = do
    return (Syntax.Alternative name)

resolve here (Syntax.Merge record₀) = do
    record₁ <- resolve here record₀

    return (Syntax.Merge record₁)

resolve _ Syntax.True = do
    return Syntax.True

resolve _ Syntax.False = do
    return Syntax.False

resolve here (Syntax.And left₀ right₀) = do
    left₁  <- resolve here left₀
    right₁ <- resolve here right₀

    return (Syntax.And left₁ right₁)

resolve here (Syntax.Or left₀ right₀) = do
    left₁  <- resolve here left₀
    right₁ <- resolve here right₀

    return (Syntax.Or left₁ right₁)

resolve here (Syntax.If predicate₀ ifTrue₀ ifFalse₀) = do
    predicate₁ <- resolve here predicate₀
    ifTrue₁    <- resolve here ifTrue₀
    ifFalse₁   <- resolve here ifFalse₀

    return (Syntax.If predicate₁ ifTrue₁ ifFalse₁)

resolve _ (Syntax.Natural number) = do
    return (Syntax.Natural number)

resolve here (Syntax.Times left₀ right₀) = do
    left₁  <- resolve here left₀
    right₁ <- resolve here right₀

    return (Syntax.Times left₁ right₁)

resolve here (Syntax.Plus left₀ right₀) = do
    left₁  <- resolve here left₀
    right₁ <- resolve here right₀

    return (Syntax.Plus left₁ right₁)

resolve _ Syntax.NaturalFold = do
    return Syntax.NaturalFold

resolve _ (Syntax.Text text) = do
    return (Syntax.Text text)

resolve here (Syntax.Append left₀ right₀) = do
    left₁  <- resolve here left₀
    right₁ <- resolve here right₀

    return (Syntax.Append left₁ right₁)

resolve here (Syntax.Embed file) = do
    let there = FilePath.takeDirectory here </> file

    result <- interpret (Path there)

    return (Syntax.Embed result)
