{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Grace.Import where

import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Grace.Pretty (Pretty(..))

import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified System.Process.Typed as Process
import qualified Text.URI             as URI

data Import
    = File FilePath
    | URI URI.URI

instance Pretty Import where
    pretty (File file) = pretty file
    pretty (URI uri) = pretty uri

resolveURI :: (MonadThrow m, MonadIO m) => URI.URI -> m Text
resolveURI uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "file") } =
    case URI.uriPath uri of
        Nothing -> throwM $ ImportException uri FileEmptyPath
        Just (_, pieces) -> return $ pathPiecesToFilePath pieces
--resolveURI uri@URI.URI{ URI.uriScheme = Just "http" } = return undefined
--resolveURI uri@URI.URI{ URI.uriScheme = Just "https" } = return undefined
resolveURI uri@URI.URI{ URI.uriScheme = Just scheme } = liftIO $ do
    let cmd = "grace-resolver-" <> Text.unpack (URI.unRText scheme)
        args = [URI.renderStr uri]
        pc  = Process.setStdout Process.createPipe
            $ Process.proc cmd args
    Process.withProcessWait_ pc $ \p -> do
        Text.IO.hGetContents $ Process.getStdout p
resolveURI uri = throwM $ ImportException uri MissingURIScheme

pathPiecesToFilePath :: NonEmpty (URI.RText 'URI.PathPiece) -> Text
pathPiecesToFilePath = foldl' f "/"
  where
    f memo x = memo <> "/" <> URI.unRText x

data ImportException = ImportException URI.URI ImportExceptionMessage
    deriving Show

instance Exception ImportException

data ImportExceptionMessage
    = FileEmptyPath
    | MissingURIScheme
    deriving Show
