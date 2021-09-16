{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- | This module contains the functions and types that power to URI-base imports
     as well as the builtin resolvers for those imports.
-}

module Grace.Import
    ( -- * Resolving imports
      Import(..)
    , ImportCallback
    , Resolver(..)
    , resolverToCallback
    , ImportError(..)

      -- * Builtin default resolver
    , defaultResolver
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
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Grace.Pretty (Pretty(..))
import System.FilePath ((</>))
import Text.URI (Authority)

import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Maybe              as Unsafe (fromJust)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text.IO
import qualified Data.Text.Lazy          as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified System.Environment      as Environment
import qualified System.IO.Error         as Error
import qualified System.Process.Typed    as Process
import qualified Text.URI                as URI

{- | A reference to external source code that will be imported by the
     interpreter.
-}
data Import
    = File FilePath
    | URI URI.URI

instance Pretty Import where
    pretty (File file) = pretty file
    pretty (URI uri) = pretty uri

-- | Type of the callback function used to resolve URI imports
type ImportCallback = URI.URI -> IO Text

{- | A resolver for an URI.

     When the interpreter tries to resolve an URI pointing to some source code
     it will try multiple resolvers sequentially and stops if one returns a
     @Just code@ value where @code@ is the source code of an expression.
     It will then try to parse and interpret that expression.

     Here are some good practices for the development of resolvers:

     * A resolver should handle exactly one URI scheme.

     * If a resolver encounters an URI which it cannot process (e.g. a
       @file://@ URI is passed to a HTTP resolver) it should return @Nothing@
       as fast as possible.

     * Exceptions thrown in resolvers will be caught and rethrown as an
       `ImportError` by the interpreter.
-}
newtype Resolver = Resolver { runResolver :: URI.URI -> IO (Maybe Text) }

instance Semigroup Resolver where
    x <> y = Resolver \uri -> do
        maybeResult <- runResolver x uri
        case maybeResult of
            Nothing -> runResolver y uri
            _ -> return maybeResult

instance Monoid Resolver where
    mempty = Resolver (const (return Nothing))

-- | Convert a resolver to a callback function
resolverToCallback :: Resolver -> ImportCallback
resolverToCallback resolver uri = do
    maybeResult <- runResolver resolver uri
    case maybeResult of
        Nothing -> throw UnsupportedURI
        Just result -> return result

-- | Errors that might be raised during import resolution.
data ImportError
    = UnsupportedURI
    deriving stock Show

instance Exception ImportError where
    displayException UnsupportedURI = "Resolving this URI is not supported"

-- Builtin resolvers

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

     It will fail if the URI has an authority component.
-}
envResolver :: Resolver
envResolver = Resolver \case
    uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "env") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw EnvUnexpectedAuthority
            _ -> return ()

        pieces <- case URI.uriPath uri of
            Nothing -> throw EnvMissingVarName
            Just (_, pieces) -> return pieces

        let n = pathPiecesToName pieces

        v <- Environment.lookupEnv (Text.unpack n) >>= \case
            Nothing -> throw (EnvVarNotFound n)
            Just v -> return v

        return (Just (Text.pack v))
    _ -> return Nothing
    where
        pathPiecesToName = Text.intercalate "_" . map URI.unRText . NonEmpty.toList

-- | Errors raised by `envResolver`
data EnvResolverError
    = EnvMissingVarName
    | EnvUnexpectedAuthority
    | EnvVarNotFound Text
    deriving stock Show

instance Exception EnvResolverError where
    displayException EnvUnexpectedAuthority = "Unexpected authority component"
    displayException (EnvVarNotFound k) = Text.unpack ("Environment variable not found: " <> k)
    displayException EnvMissingVarName = "Environment variable name is missing"

{- | A resolver for files.

     This resolver matches URIs with the @file:@ scheme. The resolver takes the
     path of the URI, tries to read its content and returns it as a result.

     It will fail if the URI has an authority component.
-}
fileResolver :: Resolver
fileResolver = Resolver \case
    uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "file") } -> do
        case URI.uriAuthority uri of
            Right auth | auth /= emptyAuthority -> throw FileUnexpectedAuthority
            _ -> return ()

        pieces <- case URI.uriPath uri of
            Nothing -> throw FileMissingPath
            Just (_, pieces) -> return pieces

        Just <$> Text.IO.readFile (pathPiecesToFilePath pieces)
    _ -> return Nothing
    where
        pathPiecesToFilePath = foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

-- | Errors raised by `fileResolver`
data FileResolverError
    = FileMissingPath
    | FileUnexpectedAuthority
    deriving stock Show

instance Exception FileResolverError where
    displayException FileMissingPath = "Filepath is missing"
    displayException FileUnexpectedAuthority = "Unexpected authority component"

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

        return (Just text)
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
