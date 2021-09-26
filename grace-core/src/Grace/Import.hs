{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

-- | This module contains the import resolution logic
module Grace.Import
    ( -- * Import resolution
      resolve
      -- * Exceptions
    , ResolutionError(..)
    , ImportError(..)
    ) where

import Control.Exception.Safe (Exception(..))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Syntax (Syntax)
import Network.HTTP.Client (Manager)
import System.FilePath ((</>))
import Text.URI (Authority)
import Text.URI.QQ (host, scheme)

import qualified Control.Exception.Safe as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import qualified Grace.Parser as Parser
import qualified Network.HTTP.Client as HTTP
import qualified System.Environment as Environment
import qualified Text.URI as URI

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Manager -> Input -> IO (Syntax Location Input)
resolve manager input = case input of
    URI uri
        | any (`elem` [ [scheme|http|], [scheme|https|] ]) (URI.uriScheme uri) -> do
            let name = URI.renderStr uri

            request <- HTTP.parseUrlThrow name

            response <- HTTP.httpLbs request manager

            let lazyBytes = HTTP.responseBody response

            code <- case Encoding.decodeUtf8' lazyBytes of
                Left exception -> throw (NotUTF8 exception)
                Right lazyText -> return (Text.Lazy.toStrict lazyText)

            result <- case Parser.parse name code of
                Left e -> Exception.throw e
                Right result -> return result

            let locate offset = Location{..}

            return (first locate result)

        | URI.uriScheme uri == Just [scheme|env|]
        , all (== emptyAuthority) (URI.uriAuthority uri) -> do
            var <- case URI.uriPath uri of
                Nothing -> throw MissingEnvironmentVariableName
                Just (False, var :| []) -> return (URI.unRText var)
                _ -> throw InvalidURI

            code <- Environment.lookupEnv (Text.unpack var) >>= \case
                Nothing -> throw MissingEnvironmentVariable
                Just string -> return (Text.pack string)

            let name = "env:" <> Text.unpack var

            result <- case Parser.parse name code of
                Left e -> Exception.throw e
                Right result -> return result

            let locate offset = Location{..}

            return (first locate result)

        | URI.uriScheme uri == Just [scheme|file|]
        , all (== emptyAuthority) (URI.uriAuthority uri) -> do
            pieces <- case URI.uriPath uri of
                Nothing -> throw MissingFile
                Just (_, pieces) -> return pieces

            let pathPiecesToFilePath =
                    foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

            readPath (pathPiecesToFilePath pieces)

        | otherwise -> do
            throw InvalidURI

    Path path -> do
        readPath path

    Code name code -> do
        result <- case Parser.parse name code of
            Left e -> Exception.throw e
            Right result -> return result

        let locate offset = Location{..}

        return (first locate result)
  where
    readPath path = do
        code <- Text.IO.readFile path

        result <- case Parser.parse path code of
            Left e -> Exception.throw e
            Right result -> return result

        let locate offset = Location{ name = path, ..}

        return (first locate result)

    throw e = Exception.throw (ImportError input e)

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = [host||]
    , URI.authPort = Nothing
    }

-- | The base error for `ImportError` (without the @input@ information)
data ResolutionError
    = InvalidURI
    | MissingEnvironmentVariableName
    | MissingEnvironmentVariable
    | MissingFile
    | NotUTF8 UnicodeException
    deriving stock (Eq, Show)

data ImportError = ImportError
    { input :: Input
    , resolutionError :: ResolutionError
    } deriving stock (Eq, Show)

instance Exception ImportError where
    displayException ImportError{..} =
        Text.unpack [__i|
        #{renderedInput}: #{renderedError}
        |]
      where
        renderedInput = case input of
            URI  uri  -> URI.render uri
            Path path -> Text.pack path
            Code _ _  -> "(input)"

        renderedError :: Text
        renderedError = case resolutionError of
            InvalidURI ->
                "Invalid URI"
            MissingEnvironmentVariableName ->
                "Missing environment variable name"
            MissingEnvironmentVariable ->
                "Missing environment variable"
            MissingFile ->
                "Missing file"
            NotUTF8 exception ->
                [__i|
                Not UTF8

                #{Exception.displayException exception}
                |]
