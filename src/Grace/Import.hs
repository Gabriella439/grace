{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | This module contains the import resolution logic
module Grace.Import
    ( -- * Import resolution
      resolve
    , referentiallySane

      -- * Exceptions
    , ResolutionError(..)
    , ImportError(..)
    ) where

import Control.Exception.Safe (Exception(..), MonadCatch)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Grace.HTTP (HttpException, Manager)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Syntax (Syntax)
import System.FilePath ((</>))
import Text.URI (Authority, RText, RTextLabel(..))

import qualified Control.Exception.Safe as Exception
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.HTTP as HTTP
import qualified Grace.Parser as Parser
import qualified Grace.Pretty as Pretty
import qualified Grace.Syntax as Syntax
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as Unsafe
import qualified Text.URI as URI

cache :: IORef (HashMap Text Text)
cache = Unsafe.unsafePerformIO (IORef.newIORef HashMap.empty)
{-# NOINLINE cache #-}

fetch :: Manager -> Text -> IO Text
fetch manager url = do
    m <- IORef.readIORef cache

    case HashMap.lookup url m of
        Nothing -> do
            body  <- HTTP.fetch manager url

            IORef.writeIORef cache $! HashMap.insert url body m

            return body
        Just body -> do
            return body

remoteSchemes :: [RText 'Scheme]
remoteSchemes = map (fromJust . URI.mkScheme) [ "http", "https" ]

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Manager -> Input -> IO (Syntax Location Input)
resolve manager input = case input of
    URI uri mode
        | any (`elem` remoteSchemes) (URI.uriScheme uri) -> do
            let name = URI.renderStr uri

            let handler e = throw (HTTPError e)

            text <- Exception.handle handler (fetch manager (Text.pack name))

            result <- case mode of
                AsCode -> case Parser.parse name text of
                    Left e -> Exception.throw e
                    Right result -> return result
                AsText -> do
                    return Syntax.Text
                        { chunks = Syntax.Chunks text []
                        , location = 0
                        }

            let locate offset = Location{ code = text, .. }

            return (first locate result)

        | URI.uriScheme uri == URI.mkScheme "env" -> do
            case URI.uriAuthority uri of
                Left False -> do
                    var <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (False, var :| []) -> return (URI.unRText var)
                        _ -> throw UnsupportedPathSeparators

                    maybeCode <- Environment.lookupEnv (Text.unpack var)

                    text <- case maybeCode of
                        Nothing -> throw MissingEnvironmentVariable
                        Just string -> return (Text.pack string)

                    let name = "env:" <> Text.unpack var

                    result <- case mode of
                        AsCode -> case Parser.parse name text of
                            Left e -> Exception.throw e
                            Right result -> return result
                        AsText -> do
                            return Syntax.Text
                                { chunks = Syntax.Chunks text []
                                , location = 0
                                }

                    let locate offset = Location{ code = text, .. }

                    return (first locate result)
                Left True -> do
                    throw UnsupportedPathSeparators
                Right _ -> do
                    throw UnsupportedAuthority

        | URI.uriScheme uri == URI.mkScheme "file" -> do
            if all (== emptyAuthority) (URI.uriAuthority uri)
                then do
                    pieces <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (_, pieces) -> return pieces

                    let pathPiecesToFilePath =
                            foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

                    readPath mode (pathPiecesToFilePath pieces)
                else do
                    throw UnsupportedAuthority

        | otherwise -> do
            throw InvalidURI

    Path path mode -> do
        readPath mode path

    Code name code -> do
        result <- case Parser.parse name code of
            Left e -> Exception.throw e
            Right result -> return result

        let locate offset = Location{..}

        return (first locate result)
  where
    readPath mode path = do
        text <- Text.IO.readFile path

        result <- case mode of
            AsCode -> case Parser.parse path text of
                Left e -> Exception.throw e
                Right result -> return result
            AsText -> do
                return Syntax.Text
                    { chunks = Syntax.Chunks text []
                    , location = 0
                    }

        let locate offset = Location{ name = path, code = text, ..}

        return (first locate result)

    throw e = Exception.throw (ImportError input e)

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = fromJust (URI.mkHost "")
    , URI.authPort = Nothing
    }

remote :: Input -> Bool
remote (URI uri _) = any (`elem` remoteSchemes) (URI.uriScheme uri)
remote _ = False

-- | Fail if the child import tries to access something that the parent import
-- should not have access to
referentiallySane :: MonadCatch m => Input -> Input -> m ()
referentiallySane parent child
    | remote parent && not (remote child) = do
        Exception.throwIO (ImportError parent (ReferentiallyInsane child))
    | otherwise = do
        return ()

-- | The base error for `ImportError` (without the @input@ information)
data ResolutionError
    = HTTPError HttpException
    | InvalidURI
    | MissingEnvironmentVariable
    | MissingPath
    | UnsupportedPathSeparators
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
        Text.unpack
            ("Import resolution failed: " <> renderedInput <> "\n\
            \\n\
            \" <> renderedError
            )
      where
        renderedInput = case input of
            URI  uri AsCode -> URI.render uri
            URI  uri AsText -> URI.render uri <> " : Text"

            Path path AsCode -> Text.pack path
            Path path AsText -> Text.pack path <> " : Text"

            Code _ _  -> "(input)"

        renderedError :: Text
        renderedError = case resolutionError of
            HTTPError httpException ->
                HTTP.renderError httpException
            InvalidURI ->
                "Invalid URI"
            MissingEnvironmentVariable ->
                "Missing environment variable"
            MissingPath ->
                "Missing path"
            ReferentiallyInsane child ->
                "Local imports are rejected within remote imports\n\
                \\n\
                \Rejected local import: " <> Text.pack (show (Pretty.pretty child))
            UnsupportedPathSeparators ->
                "Unsupported path separators"
            UnsupportedAuthority ->
                "Unsupported authority"
