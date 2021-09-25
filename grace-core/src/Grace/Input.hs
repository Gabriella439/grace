-- | This module contains the functions and types that power to URI-base imports
module Grace.Input
    ( -- * Input
      Input(..)
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Grace.Pretty (Pretty(..))
import System.FilePath ((</>))

import qualified Data.Text       as Text
import qualified System.FilePath as FilePath
import qualified Text.URI        as URI

{-| Input to the interpreter.

    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code String Text
    -- ^ Source code: @Code name content@
    | URI URI.URI
    deriving (Eq, Show)

instance Semigroup Input where
    _ <> URI uri = URI uri

    _ <> Code name code = Code name code

    Code _ _    <> Path child = Path child
    Path parent <> Path child = Path (FilePath.takeDirectory parent </> child)
    URI parent  <> Path child
        | FilePath.isRelative child
        , Just uri <- URI.relativeTo childURI parent =
            URI uri
        | otherwise =
            Path child
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
    pretty (Path path) = pretty path
    pretty (URI uri) = pretty uri
