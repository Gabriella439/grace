{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module contains the builtin resolvers for the grace executable
-}

module Grace.Resolver.Builtin
    ( -- * Builtin default resolver
      defaultResolver

      -- ** env:// resolver
    , envResolver
    , EnvResolverError(..)

      -- ** file:// resolver
    , fileResolver
    , FileResolverError(..)

      -- ** Resolver using external programs
    , externalResolver
    ) where

import Control.Exception.Safe (Exception(..), handleIO, throw)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Grace.Import (Resolver(..))
import System.FilePath ((</>))
import Text.URI (Authority)

import qualified Control.Monad.Except    as Except
import qualified Grace.Interpret         as Interpret
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Maybe              as Unsafe (fromJust)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified System.Environment      as Environment
import qualified System.IO.Error         as Error
import qualified System.Process.Typed    as Process
import qualified Text.URI                as URI

{- | A set of default resolvers. Includes (order matters):

     * `envResolver`
     * `fileResolver`
     * `externalResolver`
-}
defaultResolver :: Resolver
defaultResolver
    =  envResolver
    <> fileResolver
    <> externalResolver

{- | A resolver for environment variables.

     This resolver matches URIs with the @env:@ scheme. In order to obtain the
     name of the environment variable it takes the path component and replaces
     each \"/\" with \"_\". The value of the environment variable is the
     source code returned by this resolver.

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

        let input = Interpret.Code "(environment)" (Text.pack value)

        eitherResult <- Except.runExceptT (Interpret.parseInput input)

        result <- case eitherResult of
            Left e -> throw e
            Right result -> return result

        return (Just result)
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
     path of the URI, tries to read its content and returns it as a result.

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

        let input = Interpret.Path (pathPiecesToFilePath pieces)

        eitherResult <- Except.runExceptT (Interpret.parseInput input)

        result <- case eitherResult of
            Left e -> throw e
            Right result -> return result

        return (Just result)
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

{- | A resolver that uses external programs to resolve the URI.

     This resolver passes the URI as an command line argument to an executable
     named @grace-resolver-\<scheme\>@. If that fails because the executable
     cannot be looked up (i.e. it does not exist in @PATH@) the URI is
     considered unmatched by this resolver and @Nothing@ is returned. On
     success, the standard output of that process is expected to be the desired
     expression to be returned as a result.

     It will fail if the URI has an authority component.
-}
externalResolver :: Resolver
externalResolver = Resolver \case
    uri@URI.URI{ URI.uriScheme = Just scheme } -> handleDoesNotExist do
        let cmd = "grace-resolver-" <> Text.unpack (URI.unRText scheme)

        let args = [URI.renderStr uri]

        let pc = Process.proc cmd args

        bytes <- Process.readProcessStdout_ pc

        text <- case Text.Lazy.Encoding.decodeUtf8' bytes of
            Left e -> throw (ExternalFailedDecodeStdout e)
            Right text -> return (Text.Lazy.toStrict text)

        let input = Interpret.Code "(external)" text

        eitherResult <- Except.runExceptT (Interpret.parseInput input)

        result <- case eitherResult of
            Left e -> throw e
            Right result -> return result

        return (Just result)
    _ -> return Nothing
    where
        handleDoesNotExist = handleIO \e ->
            if Error.isDoesNotExistError e
                then return Nothing
                else throw e

-- | Errors raised by `externalResolver`
data ExternalResolverError = ExternalFailedDecodeStdout UnicodeException
    deriving stock Show

instance Exception ExternalResolverError where
    displayException (ExternalFailedDecodeStdout e) = "Failed to decode stdout: " <> displayException e

-- Internal helper functions

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = Unsafe.fromJust (URI.mkHost "")
    , URI.authPort = Nothing
    }
