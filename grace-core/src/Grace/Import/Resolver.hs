{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module contains the builtin resolvers for the grace executable
-}

module Grace.Import.Resolver
    ( -- * Builtin default resolver
      defaultResolver

      -- ** env:// resolver
    , envResolver
    , EnvResolverError(..)

      -- ** file:// resolver
    , fileResolver
    , FileResolverError(..)
    ) where

import Control.Exception.Safe (Exception(..), throw)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.Import (Resolver(..))
import System.FilePath ((</>))
import Text.URI (Authority)

import qualified Control.Monad.Except    as Except
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Text               as Text
import qualified Grace.Parser            as Parser
import qualified System.Environment      as Environment
import qualified System.FilePath         as FilePath
import qualified Text.URI                as URI
import qualified Text.URI.QQ             as URI.QQ

{- | A set of default resolvers. Includes (order matters):

     * `envResolver`
     * `fileResolver`
-}
defaultResolver :: Resolver
defaultResolver
    =  envResolver
    <> fileResolver

{- | A resolver for environment variables.

     This resolver matches URIs with the @env:@ scheme. It assumes that the
     first path component is the name of the environment variable, looks it up
     and expects the value to be an expression. It will then return the parsed
     expression as a result.

     It will fail if the URI has an authority component, a trailing slash or
     more than one path components. I.e. a valid URI looks like @env:///NAME@.
-}
envResolver :: Resolver
envResolver = Resolver \case
    uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "env") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw EnvInvalidURI
            _ -> return ()

        name <- case URI.uriPath uri of
            Nothing -> throw EnvMissingVarName
            Just (False, name :| []) -> return (URI.unRText name)
            _ -> throw EnvInvalidURI

        value <- Environment.lookupEnv (Text.unpack name) >>= \case
            Nothing -> throw (EnvVarNotFound name)
            Just value -> return value

        let input = Parser.Code ("env:" <> Text.unpack name) (Text.pack value)

        eitherResult <- Except.runExceptT (Parser.parseInput input)

        result <- case eitherResult of
            Left e -> throw e
            Right result -> return result

        return (Just (result, Nothing))
    _ -> return Nothing

-- | Errors raised by `envResolver`
data EnvResolverError
    = EnvInvalidURI
    | EnvMissingVarName
    | EnvVarNotFound Text
    deriving stock Show

instance Exception EnvResolverError where
    displayException EnvInvalidURI = "Invalid URI"
    displayException EnvMissingVarName = "Environment variable name is missing"
    displayException (EnvVarNotFound k) = Text.unpack ("Environment variable not found: " <> k)

{- | A resolver for files.

     This resolver matches URIs with the @file:@ scheme. The resolver takes the
     path of the URI, tries to read its content and returns the parsed value as
     its result.

     It will fail if the URI has an authority component.
-}
fileResolver :: Resolver
fileResolver = Resolver \case
    uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "file") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw FileInvalidURI
            _ -> return ()

        pieces <- case URI.uriPath uri of
            Nothing -> throw FileMissingPath
            Just (_, pieces) -> return pieces

        let path = pathPiecesToFilePath pieces

        let input = Parser.Path path

        eitherResult <- Except.runExceptT (Parser.parseInput input)

        result <- case eitherResult of
            Left e -> throw e
            Right result -> return result

        return (Just (result, Just (FilePath.takeDirectory path)))
    _ -> return Nothing
    where
        pathPiecesToFilePath = foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

-- | Errors raised by `fileResolver`
data FileResolverError
    = FileInvalidURI
    | FileMissingPath
    deriving stock Show

instance Exception FileResolverError where
    displayException FileInvalidURI = "Invalid URI"
    displayException FileMissingPath = "Filepath is missing"

-- Internal helper functions

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = [URI.QQ.host||]
    , URI.authPort = Nothing
    }
