module Main where

import Data.Text (Text)
import Grace.Interpret (Input(..))
import Prettyprinter (Pretty)
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Control.Monad.Except      as Except
import qualified Data.Text                 as Text
import qualified Grace.Interpret           as Interpret
import qualified Grace.Normalize           as Normalize
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified System.Directory          as Directory
import qualified System.FilePath           as FilePath
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.Silver         as Silver

pretty_ :: Pretty a => a -> Text
pretty_ x = Pretty.Text.renderStrict stream
  where
    doc = Pretty.pretty x <> Pretty.hardline

    stream = Pretty.layoutPretty Pretty.defaultLayoutOptions doc

fileToTestTree :: FilePath -> IO TestTree
fileToTestTree prefix = do
    let input              = prefix <> "-input.grace"
    let expectedTypeFile   = prefix <> "-type.grace"
    let expectedOutputFile = prefix <> "-output.grace"
    let expectedStderrFile = prefix <> "-stderr.txt"

    let name = FilePath.takeBaseName input

    eitherResult <- Except.runExceptT (Interpret.interpret (Path input))

    case eitherResult of
        Left message -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - error")
                        expectedStderrFile
                        (return message)
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
    fmap Text.unpack (Text.stripSuffix "-input.grace" (Text.pack inputFile))

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
    testTree <- directoryToTestTree "tasty/data"

    Tasty.defaultMain testTree
