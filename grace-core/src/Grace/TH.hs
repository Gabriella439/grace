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

import Control.Exception (displayException)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Grace.Interpret (Input(..))
import Grace.Syntax (Syntax)
import Grace.Type (Type)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift, Q, TExp(..))

import qualified Control.Monad.Except as Except
import qualified Data.Text            as Text
import qualified Grace.Interpret      as Interpret
import qualified Grace.Normalize      as Normalize
import qualified Language.Haskell.TH  as TH
import qualified Language.Haskell.TH.Syntax as TH

-- $setup
-- >>> :set -XOverloadedStrings -XQuasiQuotes -XTemplateHaskell

{- | A quasi-quoter for expressions.

     Takes the source code of a expression, type checks it and returns the fully
     normalized AST.

     >>> [grace| "hello" |]
     Syntax {location = (), node = Scalar (Text "hello")}

     This quoter is implemented using `expressionFromCode`.
     Note that other quoting (declarations, patterns, types) is not supported.
-}
grace :: QuasiQuoter
grace = QuasiQuoter
    { quoteExp = fmap TH.unType . expressionFromCode . Text.pack
    , quoteDec = error "Declaration quoting not supported"
    , quotePat = error "Pattern quoting not supported"
    , quoteType = error "Type quoting not supported"
    }

{- | Evaluate an expression at compile time.

     This function takes the source code of a expressions, type checks it and
     returns the fully normalized AST.

     >>> $$(expressionFromCode "\"hello\"")
     Syntax {location = (), node = Scalar (Text "hello")}
-}
expressionFromCode :: Text -> Q (TExp (Syntax () Void))
expressionFromCode = expressionFromInput . Code "(input)"

-- | Like `expressionFromCode`, but takes path of a source file as input.
expressionFromFile :: FilePath -> Q (TExp (Syntax () Void))
expressionFromFile = expressionFromInput . Path

-- | Like `expressionFromCode`, but expects `Input` as an argument.
expressionFromInput :: Input -> Q (TExp (Syntax () Void))
expressionFromInput = helperFunction snd

{- | Infer the type of an expression at compile time.

     This function takes the source code of an expressions, type checks it and
     returns the inferred type of that expression.

     >>> $$(typeOfCode "\"hello\"")
     Type {location = (), node = Scalar Text}
-}
typeOfCode :: Text -> Q (TExp (Type ()))
typeOfCode = typeOfInput . Code "(input)"

-- | Like `typeOfCode`, but takes path of a source file as input.
typeOfFile :: FilePath -> Q (TExp (Type ()))
typeOfFile = typeOfInput . Path

-- | Like `typeOfCode`, but expects `Input` as an argument.
typeOfInput :: Input -> Q (TExp (Type ()))
typeOfInput = helperFunction fst

-- Internal functions

helperFunction :: Lift r => ((Type (), Syntax () Void) -> r) -> Input -> Q (TExp r)
helperFunction f input = do
    eitherResult <- Except.runExceptT (Interpret.interpret Nothing input)

    (inferred, value) <- case eitherResult of
        Left e -> fail (displayException e)
        Right result -> return result

    let type_ = void inferred
        syntax = Normalize.quote [] value

    TExp <$> TH.lift (f (type_, syntax))
