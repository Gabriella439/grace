{-# LANGUAGE RecordWildCards #-}

{-| This module contains the `Location` type used for attaching source code to
    error messages
-}
module Grace.Location
    ( -- * Location
      Location(..)
    , Offset(..)
    , renderError
    ) where

import Data.Text (Text)
import Text.Megaparsec (PosState(..), SourcePos(..))

import qualified Data.Text as Text
import qualified Text.Megaparsec.Pos as Pos
import qualified Text.Megaparsec.Stream as Stream

-- | Offsets are stored in characters (0-indexed)
newtype Offset = Offset { getOffset :: Int }
    deriving newtype (Eq, Num, Show)

-- | This type stores the location of each subexpression
data Location
    = Location
        { name :: String
        -- ^ The file or name describing where the code came from
        , code :: Text
        -- ^ The original source code (the entire file)
        --
        --   Note that this will not always be the same for each `Location`
        --   because different subexpressions might originate from different
        --   files if they were imported
        , offset :: Offset
        -- ^ The offset (in characters) within the code
        }
    | Unknown
    deriving stock (Eq, Show)

-- | Render an error message, given a `Location` for the error
renderError
    :: Text
    -- ^ Error message
    -> Location
    -- ^ Location of the error
    -> Text
renderError message Location{..} = prefix <> "\n" <> suffix
  where
    initialState =
        PosState
            { pstateInput      = code
            , pstateOffset     = 0
            , pstateSourcePos  = Pos.initialPos name
            , pstateTabWidth   = Pos.defaultTabWidth
            , pstateLinePrefix = ""
            }

    (h, state) = Stream.reachOffset (getOffset offset) initialState

    pos = pstateSourcePos state

    line = Pos.unPos (sourceLine pos)

    column = Pos.unPos (sourceColumn pos)

    suffix = case h of
        Just string ->
            let lineText = Text.pack (show line)

                inner = lineText <> " │"

                outer = Text.replicate (Text.length lineText) " " <> " │"

                caret = Text.replicate (column - 1) " " <> "↑"

            in  outer <> "\n\
                \" <> inner <> " " <> Text.pack string <> "\n\
                \" <> outer <> " " <> caret
        Nothing ->
            ""

    prefix =
            Text.pack name
        <>  ":"
        <>  Text.pack (show line)
        <>  ":"
        <>  Text.pack (show column)
        <>  ": "
        <>  message
renderError message Unknown = message
