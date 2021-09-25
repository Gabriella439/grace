{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains the import resolution logic
module Grace.Import
    ( -- * Import resolution
      resolve

      -- * Exceptions
    , EnvResolverError(..)
    , FileResolverError(..)
    ) where

import Control.Exception.Safe (Exception(..), throw)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Syntax (Syntax)
import System.FilePath ((</>))
import Text.URI (Authority)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text.IO
import qualified Grace.Parser       as Parser
import qualified System.Environment as Environment
import qualified Text.URI           as URI
import qualified Text.URI.QQ        as URI.QQ

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Input -> IO (Syntax Location Input)
resolve input = case input of
    URI uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "env") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw EnvInvalidURI
            _ -> return ()

        var <- case URI.uriPath uri of
            Nothing -> throw EnvMissingVarName
            Just (False, var :| []) -> return (URI.unRText var)
            _ -> throw EnvInvalidURI

        code <- Environment.lookupEnv (Text.unpack var) >>= \case
            Nothing -> throw (EnvVarNotFound var)
            Just string -> return (Text.pack string)

        let name = "env:" <> Text.unpack var

        result <- case Parser.parse name code of
            Left e -> throw e
            Right result -> return result

        let locate offset = Location{..}

        return (first locate result)

    URI uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "file") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw FileInvalidURI
            _ -> return ()

        pieces <- case URI.uriPath uri of
            Nothing -> throw FileMissingPath
            Just (_, pieces) -> return pieces

        let pathPiecesToFilePath =
                foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

        readPath (pathPiecesToFilePath pieces)

    URI _ -> do
       throw FileInvalidURI

    Path path -> do
        readPath path

    Code name code -> do
        result <- case Parser.parse name code of
            Left e -> throw e
            Right result -> return result

        let locate offset = Location{..}

        return (first locate result)
  where
    readPath path = do
        code <- Text.IO.readFile path

        result <- case Parser.parse path code of
            Left e -> throw e
            Right result -> return result

        let locate offset = Location{ name = path, ..}

        return (first locate result)

-- | Errors raised by @env:@ imports
data EnvResolverError
    = EnvInvalidURI
    | EnvMissingVarName
    | EnvVarNotFound Text
    deriving stock Show

instance Exception EnvResolverError where
    displayException EnvInvalidURI = "Invalid URI"
    displayException EnvMissingVarName = "Environment variable name is missing"
    displayException (EnvVarNotFound k) = Text.unpack ("Environment variable not found: " <> k)

-- | Errors raised by @file:@ imports
data FileResolverError
    = FileInvalidURI
    | FileMissingPath
    deriving stock Show

instance Exception FileResolverError where
    displayException FileInvalidURI = "Invalid URI"
    displayException FileMissingPath = "Filepath is missing"

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = [URI.QQ.host||]
    , URI.authPort = Nothing
    }
