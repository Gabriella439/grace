{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

-- | This module implements the main interpretation function
module Grace.Interpret
    ( -- * Interpret
      Input(..)
    , ImportCallback
    , interpret
    , interpretWith
    , interpretExprWith

      -- * Errors related to interpretation
    , InterpretError(..)
    ) where

import Control.Exception.Safe (Exception(..), tryAny)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (Bifunctor(..))
import Data.Generics.Product (the)
import Data.Text (Text)
import Data.Traversable (for)
import Grace.Import (Import, ImportCallback)
import Grace.Location (Location(..))
import Grace.Parser (Input(..))
import Grace.Syntax (Node(..), Syntax(..))
import Grace.Type (Type)
import Grace.Value (Value)
import System.FilePath ((</>))

import qualified Control.Lens          as Lens
import qualified Control.Monad.Except  as Except
import qualified Data.Text             as Text
import qualified Grace.Context         as Context
import qualified Grace.Import          as Import
import qualified Grace.Import.Resolver as Resolver
import qualified Grace.Infer           as Infer
import qualified Grace.Normalize       as Normalize
import qualified Grace.Parser          as Parser
import qualified Grace.Syntax          as Syntax
import qualified System.FilePath       as FilePath
import qualified Text.URI              as URI

{-| Interpret Grace source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Grace interpreter
-}
interpret
    :: (MonadError InterpretError m, MonadIO m)
    => Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpret = interpretWith []

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadError InterpretError m, MonadIO m)
    => [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Input
    -> m (Type Location, Value)
interpretWith bindings maybeAnnotation input = do
    let directory = case input of
            Path file -> FilePath.takeDirectory file
            Code _ _  -> "."

    eitherExpression <- Except.runExceptT (Parser.parseInput input)

    expression <- case eitherExpression of
        Left e -> throwError (ParseError e)
        Right expression -> return expression

    interpretExprWith bindings maybeAnnotation directory expression

{- | Like `interpretWith`, but takes a pre-parsed expression instead of some
     source code
-}
interpretExprWith
    :: (MonadError InterpretError m, MonadIO m)
    => [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> FilePath
    -- ^ The base directory used when resolving filepath imports of relative paths
    -> Syntax Location Import
    -> m (Type Location, Value)
interpretExprWith bindings maybeAnnotation directory expression = do
    resolvedExpression <- for (annotate expression)
        \(maybeAnnotation', input) -> case input of
            Import.File file -> interpretWith bindings maybeAnnotation' (Path path)
              where
                path = FilePath.normalise (directory </> file)

            Import.URI uri -> do
                eitherResult <- liftIO do
                    -- CUSTOMIZE ME
                    -- If you want support for other URI types you probably want
                    -- to replace the default resolver with your own one.
                    tryAny (Import.resolverToCallback Resolver.defaultResolver uri)

                importExpression <- case eitherResult of
                    Left e -> throwError (ImportError uri (displayException e))
                    Right result -> return result

                let relocate location = location
                        { name = Text.unpack (URI.render uri)
                        }

                let relocatedExpression = first relocate importExpression

                interpretExprWith bindings maybeAnnotation' directory relocatedExpression

    let annotatedExpression =
            case maybeAnnotation of
                Nothing         -> resolvedExpression
                Just annotation ->
                    Syntax
                        { node = Annotation resolvedExpression annotation
                        , location = Syntax.location resolvedExpression
                        }

    let typeContext = do
            (variable, type_, _) <- bindings

            return (Context.Annotation variable type_)

    case Infer.typeWith typeContext annotatedExpression of
        Left message -> do
            Except.throwError (TypeInferenceError message)

        Right inferred -> do
            let evaluationContext = do
                    (variable, _, value) <- bindings

                    return (variable, value)

            return (inferred, Normalize.evaluate evaluationContext resolvedExpression)

{-| We use this utility so that when we resolve an import of the form:

    > ./someImport.ffg : SomeType

    … then the type-annotation is used when type-checking the import.  This
    allows the user to supply an expected type to fix imports that would
    otherwise not type-check in isolation.  You can think of this function as
    \"pushing\" the type annotation into the imported expression.

    This is particularly useful when importing JSON.  For example, suppose
    that we had the following JSON expression:

    > [ 1, true ]

    We can't interpret that directly because it is a type error, and we also
    can't import that without a type annotation for the same reason.  However,
    we can import the JSON like this:

    > ./example.json : List (exists (a : Type) . a)

    … and the expression will succeed since the type annotation is used when
    type-checking @./example.json@.  We wouldn't be able to add that same type
    annotation directly to @./example.json@ because then it would no longer be
    valid JSON.
-}
annotate :: Syntax s a -> Syntax s (Maybe (Type s), a)
annotate = Lens.transform transformSyntax . fmap ((,) Nothing)
  where
    transformSyntax = Lens.over (the @"node") transformNode

    transformNode (Annotation Syntax{ node = Embed (_, a) } annotation) =
        Embed (Just annotation, a)
    transformNode node =
        node

-- | Errors related to interpretation of an expression
data InterpretError
    = ImportError URI.URI String
    | ParseError Parser.ParseError
    | TypeInferenceError Infer.TypeInferenceError
    deriving stock (Eq, Show)

instance Exception InterpretError where
    displayException (ImportError uri e) =
        "The import of " <> Text.unpack (URI.render uri) <> " failed with: " <>
        e
    displayException (ParseError e) = displayException e
    displayException (TypeInferenceError e) = displayException e
