-- | Pretty-printing logic
module Grace.Pretty
    ( -- * Prettyprinting
      renderStrict
    , renderIO
    , defaultColumns
    ) where

import Data.Text (Text)
import Prettyprinter (Doc, LayoutOptions(..), PageWidth(..))
import System.IO (Handle)

import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text

{-| Convenient wrapper around
    "Prettyprinter.Render.Text".`Pretty.Text.renderStrict`
-}
renderStrict
    :: Int
    -- ^ Available columns
    -> Doc a
    -> Text
renderStrict columns =
    Pretty.Text.renderStrict . Pretty.layoutSmart (layoutOptions columns)

{-| Convenient wrapper around
    "Prettyprinter.Render.Text".`Pretty.Text.renderIO`
-}
renderIO
    :: Int
    -- ^ Available columns
    -> Handle
    -> Doc a
    -> IO ()
renderIO columns handle =
    Pretty.Text.renderIO handle . Pretty.layoutSmart (layoutOptions columns)

-- | The default column size to use
defaultColumns :: Int
defaultColumns = 80

layoutOptions
    :: Int
    -- ^ Available columns
    -> LayoutOptions
layoutOptions columns =
    LayoutOptions { layoutPageWidth = AvailablePerLine columns 1 }
