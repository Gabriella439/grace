{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the functions and types that power to URI-base imports
module Grace.Input
    ( -- * Input
      Input(..)
    , Mode(..)
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.Pretty (Pretty(..))
import System.FilePath ((</>))

import qualified Data.Text as Text
import qualified Grace.Pretty as Pretty
import qualified System.FilePath as FilePath
import qualified Text.URI as URI

{-| Input to the interpreter.

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath Mode
    -- ^ The path to the code
    | Code String Text
    -- ^ Source code: @Code name content@
    | URI URI.URI Mode
    deriving (Eq, Show)

instance Semigroup Input where
    _ <> URI uri mode = URI uri mode

    _ <> Code name code = Code name code

    Code _ _    <> Path child mode = Path child mode
    Path parent _ <> Path child mode =
        Path (FilePath.takeDirectory parent </> child) mode
    URI parent _ <> Path child mode
        | FilePath.isRelative child
        , Just uri <- URI.relativeTo childURI parent =
            URI uri mode
        | otherwise =
            Path child mode
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
    pretty (Path path mode) = pretty path <> pretty mode
    pretty (URI uri mode) = pretty uri <> pretty mode

-- | How the imported string is interpreted
data Mode
    = AsCode
    -- ^ Interpret the string as Grace code (the default)
    | AsText
    -- ^ Interpret the string as raw text
    deriving (Eq, Show)

instance Pretty Mode where
    pretty AsCode = mempty
    pretty AsText =
        " " <> Pretty.punctuation ":" <> " " <> Pretty.builtin "Text"
