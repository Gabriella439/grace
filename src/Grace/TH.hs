{- | This module provides Template Haskell functions to embed expression and
     their times at compile-time.
-}

module Grace.TH
    ( grace
      -- * Embedding an expression
    , expressionFromCode
    , expressionFromFile
    , expressionFromInput
      -- * Embedding the type of an expression
    , typeOfCode
    , typeOfFile
    , typeOfInput
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Input (Input(..), Mode(..))
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Code(examineCode), Lift, Q, TExp(..))
import Prelude hiding (exp)

import qualified Data.Text as Text
import qualified Grace.Interpret as Interpret
import qualified Grace.Normalize as Normalize
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

-- $setup
-- >>> :set -XOverloadedStrings -XQuasiQuotes -XTemplateHaskell

{- | A quasi-quoter for expressions.

     Takes the source code of a expression, type checks it and returns the fully
     normalized AST.

     >>> [grace| "hello" |]
     Text {location = (), chunks = Chunks "hello" []}

     This quoter is implemented using `expressionFromCode`.
     Note that other quoting (declarations, patterns, types) is not supported.
-}
grace :: QuasiQuoter
grace = QuasiQuoter
    { quoteExp = fmap TH.unType . examineCode . expressionFromCode . Text.pack
    , quoteDec = error "Declaration quoting not supported"
    , quotePat = error "Pattern quoting not supported"
    , quoteType = error "Type quoting not supported"
    }

{- | Evaluate an expression at compile time.

     This function takes the source code of a expressions, type checks it and
     returns the fully normalized AST.

     >>> $$(expressionFromCode "\"hello\"")
     Text {location = (), chunks = Chunks "hello" []}
-}
expressionFromCode :: Text -> Code Q (Syntax () Void)
expressionFromCode code = expressionFromInput (Code "(quasiquote)" code)

-- | Like `expressionFromCode`, but takes path of a source file as input.
expressionFromFile :: FilePath -> Code Q (Syntax () Void)
expressionFromFile path = expressionFromInput (Path path AsCode)

-- | Like `expressionFromCode`, but expects `Input` as an argument.
expressionFromInput :: Input -> Code Q (Syntax () Void)
expressionFromInput = helperFunction snd

{- | Infer the type of an expression at compile time.

     This function takes the source code of an expressions, type checks it and
     returns the inferred type of that expression.

     >>> $$(typeOfCode "\"hello\"")
     Scalar {location = (), scalar = Text}
-}
typeOfCode :: Text -> Code Q (Type ())
typeOfCode = typeOfInput . Code "(input)"

-- | Like `typeOfCode`, but takes path of a source file as input.
typeOfFile :: FilePath -> Code Q (Type ())
typeOfFile path = typeOfInput (Path path AsCode)

-- | Like `typeOfCode`, but expects `Input` as an argument.
typeOfInput :: Input -> Code Q (Type ())
typeOfInput = helperFunction fst

-- | Underlying code for all TemplateHaskell utilities
helperFunction
    :: Lift result
    => ((Type (), Syntax () Void) -> result) -> Input -> Code Q result
helperFunction f input = TH.Code do
    (inferred, value) <- liftIO (Interpret.interpret input)

    let type_ = void inferred

    let syntax = Normalize.quote value

    exp <- TH.lift (f (type_, syntax))

    return (TExp exp)
