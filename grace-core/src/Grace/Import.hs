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
import System.FilePath ((</>))
import Text.URI (Authority)
import Text.URI.QQ (host, scheme)

import Network.HTTP.Client
    ( HttpException(..)
    , HttpExceptionContent(..)
    , Manager
    )

import qualified Control.Exception.Safe as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Grace.Parser as Parser
import qualified Grace.Pretty as Pretty
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP.Types
import qualified System.Environment as Environment
import qualified Text.URI as URI

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Manager -> Input -> IO (Syntax Location Input)
resolve manager input = case input of
    URI uri
        | any (`elem` [ [scheme|http|], [scheme|https|] ]) (URI.uriScheme uri) -> do
            let name = URI.renderStr uri

            request <- HTTP.parseUrlThrow name

            let handler e = throw (HTTPError e)

            response <- Exception.handle handler (HTTP.httpLbs request manager)

            let lazyBytes = HTTP.responseBody response

            code <- case Lazy.Encoding.decodeUtf8' lazyBytes of
                Left exception -> throw (NotUTF8 exception)
                Right lazyText -> return (Text.Lazy.toStrict lazyText)

            result <- case Parser.parse name code of
                Left e -> Exception.throw e
                Right result -> return result

            let locate offset = Location{..}

            return (first locate result)

        | URI.uriScheme uri == Just [scheme|env|] -> do
            case URI.uriAuthority uri of
                Left False -> do
                    var <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (False, var :| []) -> return (URI.unRText var)
                        _ -> throw UnsupportedPathSeparators

                    maybeCode <- Environment.lookupEnv (Text.unpack var)

                    code <- case maybeCode of
                        Nothing -> throw MissingEnvironmentVariable
                        Just string -> return (Text.pack string)

                    let name = "env:" <> Text.unpack var

                    result <- case Parser.parse name code of
                        Left e -> Exception.throw e
                        Right result -> return result

                    let locate offset = Location{..}

                    return (first locate result)
                Left True -> do
                    throw UnsupportedPathSeparators
                Right _ -> do
                    throw UnsupportedAuthority

        | URI.uriScheme uri == Just [scheme|file|] -> do
            if all (== emptyAuthority) (URI.uriAuthority uri)
                then do
                    pieces <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (_, pieces) -> return pieces

                    let pathPiecesToFilePath =
                            foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

                    readPath (pathPiecesToFilePath pieces)
                else do
                    throw UnsupportedAuthority

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
    = HTTPError HttpException
    | InvalidURI
    | MissingEnvironmentVariable
    | MissingPath
    | UnsupportedPathSeparators
    | NotUTF8 UnicodeException
    | ReferentiallyInsane Input
    | UnsupportedAuthority
    deriving stock (Show)

-- | Errors related to import resolution
data ImportError = ImportError
    { input :: Input
    , resolutionError :: ResolutionError
    } deriving stock (Show)

instance Exception ImportError where
    displayException ImportError{..} =
        Text.unpack [__i|
        Import resolution failed: #{renderedInput}

        #{renderedError}
        |]
      where
        renderedInput = case input of
            URI  uri  -> URI.render uri
            Path path -> Text.pack path
            Code _ _  -> "(input)"

        renderedError :: Text
        renderedError = case resolutionError of
            HTTPError (InvalidUrlException _ _) ->
                "Invalid URL"

            HTTPError httpException@(HttpExceptionRequest _ e) -> case e of
                ConnectionFailure _ ->
                    "Remote host not found"
                InvalidDestinationHost _ ->
                    "Invalid remote host name"
                ResponseTimeout ->
                    "The remote host took too long to respond"
                ConnectionTimeout ->
                    "Connection establishment took too long"
                StatusCodeException response body -> prefix <> suffix
                  where
                    statusCode =
                        HTTP.Types.statusCode (HTTP.responseStatus response)

                    prefix =
                        case statusCode of
                            401 -> "Access unauthorized"
                            403 -> "Access forbidden"
                            404 -> "Remote file not found"
                            500 -> "Server-side failure"
                            502 -> "Upstream failure"
                            503 -> "Server temporarily unavailable"
                            504 -> "Upstream timeout"
                            _   -> "HTTP request failure"

                    suffix =
                            "\n\n"
                        <>  [__i|
                            HTTP status code: #{statusCode}#{responseBody}
                            |]

                    responseBody :: Text
                    responseBody =
                        case Encoding.decodeUtf8' body of
                            Left _ ->
                                    "\n\n"
                                <>  [__i|
                                    Response body (non-UTF8 bytes):

                                    #{body}
                                    |]
                            Right "" ->
                                ""
                            Right bodyText ->
                                    "\n\n"
                                <>  [__i|
                                    Response body:

                                    #{prefixedText}
                                    |]
                              where
                                prefixedLines =
                                        zipWith combine prefixes
                                            (Text.lines bodyText)
                                    <>  [ "…│ …" ]
                                  where
                                    prefixes = [(1 :: Int)..7]

                                    combine n line =
                                        Text.pack (show n) <> "│ " <> line

                                prefixedText = Text.unlines prefixedLines
                _ ->
                   [__i|
                   HTTP request failure

                   #{displayException httpException}
                   |]

            InvalidURI ->
                "Invalid URI"
            MissingEnvironmentVariable ->
                "Missing environment variable"
            MissingPath ->
                "Missing path"
            NotUTF8 exception ->
                [__i|
                Not UTF8

                #{Exception.displayException exception}
                |]
            ReferentiallyInsane child ->
                [__i|
                Local imports are rejected within remote imports

                Rejected local import: #{Pretty.pretty child}
                |]
            UnsupportedPathSeparators ->
                "Unsupported path separators"
            UnsupportedAuthority ->
                "Unsupported authority"
