{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Grace.Import where

import Control.Exception.Safe (Exception, MonadThrow, SomeException, catchAny, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
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

newtype Resolver = Resolver { runResolver :: URI.URI -> IO (Maybe Text) }

findResolver :: (MonadIO m, MonadThrow m) => URI.URI -> [Resolver] -> m Text
findResolver uri [] = throwM $ MissingResolver uri
findResolver uri (resolver:xs) = do
    mout <- liftIO $ runResolver resolver uri `catchAny` \e ->
        throwM (ResolverException uri e)
    case mout of
        Nothing -> findResolver uri xs
        Just out -> return out

data ImportException
    = MissingResolver URI.URI
    | ResolverException URI.URI SomeException
    deriving Show

instance Exception ImportException

defaultResolvers :: [Resolver]
defaultResolvers =
    [ fileResolver
    , externalResolver
    ]

fileResolver :: Resolver
fileResolver = Resolver $ \case
    uri@URI.URI{ URI.uriScheme = Just (URI.unRText -> "file") } -> do
        case URI.uriPath uri of
            Nothing -> throwM MissingPath
            Just (_, pieces) -> return $ Just $ pathPiecesToFilePath pieces
    _ -> return Nothing
    where
        pathPiecesToFilePath = foldl' (\memo x -> memo <> "/" <> URI.unRText x) "/"

data FileResolverException = MissingPath
    deriving Show

instance Exception FileResolverException

--resolveURI uri@URI.URI{ URI.uriScheme = Just "http" } = return undefined
--resolveURI uri@URI.URI{ URI.uriScheme = Just "https" } = return undefined

externalResolver :: Resolver
externalResolver = Resolver $ \case
    uri@URI.URI{ URI.uriScheme = Just scheme } -> liftIO $ do
        let cmd = "grace-resolver-" <> Text.unpack (URI.unRText scheme)
            args = [URI.renderStr uri]
            pc  = Process.setStdout Process.createPipe
                $ Process.proc cmd args
        Process.withProcessWait_ pc $ \p -> do
            Just <$> Text.IO.hGetContents (Process.getStdout p)
    _ -> return Nothing
