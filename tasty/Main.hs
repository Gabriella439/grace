module Main where

import Data.Text (Text)
import Prettyprinter (Pretty)
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified Grace.Import              as Import
import qualified Grace.Infer               as Infer
import qualified Grace.Normalize           as Normalize
import qualified Grace.Parser              as Parser
import qualified Prettyprinter             as Pretty
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified System.Directory          as Directory
import qualified System.Exit               as Exit
import qualified System.FilePath           as FilePath
import qualified System.IO                 as IO
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

    let name = FilePath.takeBaseName input

    text <- Text.IO.readFile input

    expression <- case Parser.parse input text of
        Left message -> do
            Text.IO.hPutStrLn IO.stderr message
            Exit.exitFailure
        Right expression -> do
            return expression

    resolvedExpression <- Import.resolve input expression

    let generateTypeFile = do
            case Infer.typeOf resolvedExpression of
                Left message -> do
                    Text.IO.hPutStrLn IO.stderr message
                    Exit.exitFailure
                Right inferred -> return (pretty_ inferred)

    let generateOutputFile = do
            return (pretty_ (Normalize.normalize resolvedExpression))

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
    testTree <- directoryToTestTree ("tasty" </> "data")

    Tasty.defaultMain testTree
