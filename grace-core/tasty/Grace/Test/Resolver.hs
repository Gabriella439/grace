{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Grace.Test.Resolver
    ( interpretCodeWithEnvURI
    , interpretCodeWithFileURI
    ) where

import Grace.Interpret (Input(..), InterpretError)
import Grace.Location (Location(..))
import Grace.Type (Type(..))
import Test.Tasty (TestTree)

import qualified Control.Monad.Except as Except
import qualified Data.Text            as Text
import qualified Grace.Interpret      as Interpret
import qualified Grace.Monotype       as Monotype
import qualified Grace.Syntax         as Syntax
import qualified Grace.Type           as Type
import qualified Grace.Value          as Value
import qualified System.Directory     as Directory
import qualified System.Environment   as Environment
import qualified Test.Tasty.HUnit     as Tasty.HUnit

interpret :: Input -> IO (Either InterpretError (Type Location, Value.Value))
interpret input = Except.runExceptT (Interpret.interpret Nothing input)

interpretCodeWithEnvURI :: TestTree
interpretCodeWithEnvURI = Tasty.HUnit.testCase "interpret code with env:// import" do
    let uri = "env:///GRACE_TEST_VAR"

    Environment.setEnv "GRACE_TEST_VAR" "true"
    actualValue <- interpret (Code "(input)" (Text.pack uri))
    Environment.unsetEnv "GRACE_TEST_VAR"

    let expectedValue =
            Right (Type{ location, node }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name = uri, code = "true", offset = 0 }

            node = Type.Scalar Monotype.Bool

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithFileURI :: TestTree
interpretCodeWithFileURI = Tasty.HUnit.testCase "interpret code with file:// import" do
    absolute <- Directory.makeAbsolute "./tasty/data/true.ffg"

    let uri = "file://" <> absolute

    actualValue <- interpret (Code "(input)" (Text.pack uri))

    let expectedValue =
            Right (Type{ location, node }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name = uri, code = "true\n", offset = 0 }

            node = Type.Scalar Monotype.Bool

    Tasty.HUnit.assertEqual "" expectedValue actualValue
