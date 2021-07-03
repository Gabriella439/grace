{-# LANGUAGE FlexibleInstances #-}

-- | Pretty-printing logic
module Grace.Pretty
    ( -- * Prettyprinting
      renderStrict
    , renderIO
    , defaultColumns
    , Pretty(..)

      -- * Highlighting
    , keyword
    , punctuation
    , label
    , scalar
    , builtin
    , operator
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, LayoutOptions(..), PageWidth(..))
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.IO (Handle)

import qualified Prettyprinter                 as Pretty
import qualified Prettyprinter.Render.Terminal as Pretty.Terminal
import qualified Prettyprinter.Render.Text     as Pretty.Text

{-| Convenient wrapper around
    "Prettyprinter.Render.Terminal".`Pretty.Terminal.renderStrict`
    / "Prettyprinter.Render.Text".`Pretty.Text.renderStrict`
-}
renderStrict
    :: Bool
    -- ^ True to enable syntax highlighting
    -> Int
    -- ^ Available columns
    -> Doc AnsiStyle
    -> Text
renderStrict highlight columns =
    render . Pretty.layoutSmart (layoutOptions columns)
  where
    render =
        if highlight
        then Pretty.Terminal.renderStrict
        else Pretty.Text.renderStrict

{-| Convenient wrapper around
    "Prettyprinter.Render.Terminal".`Pretty.Terminal.renderIO`
    / "Prettyprinter.Render.Text".`Pretty.Text.renderIO`
-}
renderIO
    :: Bool
    -- ^ True to enable syntax highlighting
    -> Int
    -- ^ Available columns
    -> Handle
    -> Doc AnsiStyle
    -> IO ()
renderIO highlight columns handle =
    render handle . Pretty.layoutSmart (layoutOptions columns)
  where
    render =
        if highlight
        then Pretty.Terminal.renderIO
        else Pretty.Text.renderIO

-- | The default column size to use
defaultColumns :: Int
defaultColumns = 80

{-| This is like @"Prettyprinter".`Prettyprinter.Pretty`@, except that this
    can return a `Doc` with `AnsiStyle` annotations
-}
class Pretty a where
    pretty :: a -> Doc AnsiStyle

instance Pretty Double where
    pretty = Pretty.pretty

instance Pretty Int where
    pretty = Pretty.pretty

instance Pretty Integer where
    pretty = Pretty.pretty

instance Pretty Natural where
    pretty = Pretty.pretty

instance Pretty Text where
    pretty = Pretty.pretty

instance Pretty () where
    pretty = Pretty.pretty

instance Pretty Void where
    pretty = Pretty.pretty

instance Pretty String where
    pretty = Pretty.pretty

instance Pretty (Doc AnsiStyle) where
    pretty = id

layoutOptions
    :: Int
    -- ^ Available columns
    -> LayoutOptions
layoutOptions columns =
    LayoutOptions { layoutPageWidth = AvailablePerLine columns 1 }

-- | Highlight a keyword (e.g. @let@ or @merge@)
keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword =
    Pretty.annotate
        (   Pretty.Terminal.bold
        <>  Pretty.Terminal.colorDull Pretty.Terminal.Green
        )

-- | Highlight punctuation (e.g. @{@, or @,@}
punctuation :: Doc AnsiStyle -> Doc AnsiStyle
punctuation =
    Pretty.annotate
        (   Pretty.Terminal.bold
        <>  Pretty.Terminal.colorDull Pretty.Terminal.Green
        )

-- | Highlight a label (e.g. @x@)
label :: Doc AnsiStyle -> Doc AnsiStyle
label = Pretty.annotate mempty

-- | Highlight a scalar (e.g. @1@ or @\"abc\"@)
scalar :: Doc AnsiStyle -> Doc AnsiStyle
scalar = Pretty.annotate (Pretty.Terminal.colorDull Pretty.Terminal.Magenta)

-- | Highlight a built-in (e.g. @List/length@)
builtin :: Doc AnsiStyle -> Doc AnsiStyle
builtin = Pretty.annotate Pretty.Terminal.underlined

-- | Highlight an operator (e.g. @+@ or @&&@)
operator :: Doc AnsiStyle -> Doc AnsiStyle
operator =
    Pretty.annotate
        (   Pretty.Terminal.bold
        <>  Pretty.Terminal.colorDull Pretty.Terminal.Green
        )
