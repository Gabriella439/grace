{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module contains the functions and types that power to URI-base imports
-}

module Grace.Import
    ( Input(..)
    , Resolver(..)
    , resolverToCallback
    , ImportError(..)
    ) where

import Control.Exception.Safe (Exception(..), throw)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.Location (Location)
import Grace.Pretty (Pretty(..))
import Grace.Syntax (Syntax)
import System.FilePath ((</>))

import qualified Data.Text       as Text
import qualified System.FilePath as FilePath
import qualified Text.URI        as URI

{-| Input to the interpreter.

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code String Text
    -- ^ Source code: @Code name content@
    | URI URI.URI
    deriving (Eq, Show)

instance Semigroup Input where
    _ <> URI uri = URI uri

    _ <> Code name code = Code name code

    Code _ _    <> Path child = Path child
    Path parent <> Path child = Path (FilePath.takeDirectory parent </> child)
    URI parent  <> Path child
        | FilePath.isRelative child
        , Just uri <- URI.relativeTo childURI parent =
            URI uri
        | otherwise =
            Path child
      where
        uriPath = do
            c : cs <- traverse (URI.mkPathPiece . Text.pack) (FilePath.splitPath child)

            return (FilePath.hasTrailingPathSeparator child, c :| cs)

        childURI =
            URI.URI
                { URI.uriScheme = Nothing
                , URI.uriAuthority = Left False
                , URI.uriPath = uriPath
                , URI.uriQuery = []
                , URI.uriFragment = Nothing
                }

instance Pretty Input where
    pretty (Code _ code) = pretty code
    pretty (Path path) = pretty path
    pretty (URI uri) = pretty uri

{- | A resolver for a URI.

     When the interpreter tries to resolve a URI pointing to some source code
     it will try multiple resolvers sequentially and stops if one returns a
     @Just code@ value where @code@ is the source code of an expression.
     It will then try to parse and interpret that expression.

     Here are some good practices for the development of resolvers:

     * A resolver should handle exactly one URI scheme.

     * If a resolver encounters a URI which it cannot process (e.g. a
       @file://@ URI is passed to a HTTP resolver) it should return @Nothing@
       as fast as possible.

     * Exceptions thrown in resolvers will be caught and rethrown as an
       `ImportError` by the interpreter.
-}
newtype Resolver = Resolver
    { runResolver :: Input -> IO (Maybe (Syntax Location Input))
    }

instance Semigroup Resolver where
    x <> y = Resolver \uri -> do
        maybeResult <- runResolver x uri
        case maybeResult of
            Nothing -> runResolver y uri
            _ -> return maybeResult

instance Monoid Resolver where
    mempty = Resolver (const (return Nothing))

-- | Convert a resolver to a callback function
resolverToCallback :: Resolver -> Input -> IO (Syntax Location Input)
resolverToCallback resolver uri = do
    maybeResult <- runResolver resolver uri
    case maybeResult of
        Nothing -> throw UnsupportedInput
        Just result -> return result

-- | Errors that might be raised during import resolution.
data ImportError
    = UnsupportedInput
    deriving stock Show

instance Exception ImportError where
    displayException UnsupportedInput = "Resolving this input is not supported"
