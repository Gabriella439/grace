{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (displayException)
import Data.Text (Text)
import Grace.Interpret (Input(..), InterpretError)
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Resolver.Builtin
import Grace.Type (Type(..))
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Control.Monad.Except as Except
import qualified Data.Text            as Text
import qualified Grace.Import         as Import
import qualified Grace.Interpret      as Interpret
import qualified Grace.Monotype       as Monotype
import qualified Grace.Normalize      as Normalize
import qualified Grace.Pretty
import qualified Grace.Syntax         as Syntax
import qualified Grace.Type           as Type
import qualified Grace.Value          as Value
import qualified Prettyprinter        as Pretty
import qualified System.Directory     as Directory
import qualified System.Environment   as Environment
import qualified System.FilePath      as FilePath
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty.HUnit
import qualified Test.Tasty.Silver    as Silver

pretty_ :: Pretty a => a -> Text
pretty_ x =
    Grace.Pretty.renderStrict False Grace.Pretty.defaultColumns
        (pretty x <> Pretty.hardline)

interpret :: Input -> IO (Either InterpretError (Type Location, Value.Value))
interpret input = Except.runExceptT (Interpret.interpretWith importCb [] Nothing input)
    where
        importCb = Import.resolverToCallback
            (  envResolver
            <> fileResolver
            )

fileToTestTree :: FilePath -> IO TestTree
fileToTestTree prefix = do
    let input              = prefix <> "-input.ffg"
    let expectedTypeFile   = prefix <> "-type.ffg"
    let expectedOutputFile = prefix <> "-output.ffg"
    let expectedStderrFile = prefix <> "-stderr.txt"

    let name = FilePath.takeBaseName input

    eitherResult <- interpret (Path input)

    case eitherResult of
        Left e -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - error")
                        expectedStderrFile
                        (return (Text.pack (displayException e)))
                        id
                    ]
                )
        Right (inferred, value) -> do
            let generateTypeFile = return (pretty_ inferred)

            let generateOutputFile = return (pretty_ (Normalize.quote [] value))

            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - type")
                        expectedTypeFile
                        generateTypeFile
                        id
                    , Silver.goldenVsAction
                        (name <> " - output")
                        expectedOutputFile
                        generateOutputFile
                        id
                    ]
                )

inputFileToPrefix :: FilePath -> Maybe FilePath
inputFileToPrefix inputFile =
    fmap Text.unpack (Text.stripSuffix "-input.ffg" (Text.pack inputFile))

directoryToTestTree :: FilePath -> IO TestTree
directoryToTestTree directory = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToTestTree childPath

                    return [ testTree ]

                else do
                    case inputFileToPrefix childPath of
                        Just prefix -> do
                            testTree <- fileToTestTree prefix

                            return [ testTree ]

                        Nothing -> do
                            return [ ]

    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))

main :: IO ()
main = do
    autogeneratedTestTree <- directoryToTestTree "tasty/data"

    let manualTestTree =
            Tasty.testGroup "Manual tests"
                [ interpretCodeWithEnvURI
                , interpretCodeWithFileURI
                ]

    let tests = Tasty.testGroup "Tests" [ autogeneratedTestTree, manualTestTree ]

    Tasty.defaultMain tests

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
