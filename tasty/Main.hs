{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Control.Exception.Safe (Exception, SomeException)
import Data.Aeson (Value)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key, ToGraceType)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Type (Type(..))
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified Grace.Decode as Decode
import qualified Grace.Interpret as Interpret
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified Prettyprinter as Pretty
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Test.Tasty.Silver as Silver

pretty_ :: Pretty a => a -> Text
pretty_ x =
    Grace.Pretty.renderStrict False Width.defaultWidth
        (pretty x <> Pretty.hardline)

interpret :: Input -> IO (Either SomeException (Type Location, Value.Value))
interpret input = Exception.try (Interpret.interpret input)

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    result <- io

    case result of
        Left  e -> Exception.throw e
        Right a -> return a

fileToTestTree :: FilePath -> IO TestTree
fileToTestTree prefix = do
    let input              = prefix <> "-input.ffg"
    let expectedTypeFile   = prefix <> "-type.ffg"
    let expectedOutputFile = prefix <> "-output.ffg"
    let expectedStderrFile = prefix <> "-stderr.txt"

    let name = FilePath.takeBaseName input

    eitherResult <- interpret (Path input AsCode)

    case eitherResult of
        Left e -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - error")
                        expectedStderrFile
                        (return (Text.pack (Exception.displayException e)))
                        id
                    ]
                )
        Right (inferred, value) -> do
            let generateTypeFile = return (pretty_ inferred)

            let generateOutputFile = return (pretty_ value)

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

data T0
    = C0
    | C1{ foo :: Text }
    | C2{ bar :: Natural, baz :: Maybe Bool }
    | C3{ a :: Maybe Int, b :: Maybe Int, c :: Maybe Int }
    | C4{ a :: Maybe Int, b :: Maybe Int, c :: Maybe Int, d :: Maybe Int }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromGrace, ToGraceType)

data T1
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

main :: IO ()
main = do
    autogeneratedTestTree <- directoryToTestTree "tasty/data"

    let manualTestTree =
            Tasty.testGroup "Manual tests"
                [ interpretCode
                , interpretCodeWithEnvURI
                , interpretCodeWithFileURI
                , interpretCodeWithImport
                , decodeWithTypeError
                , decodeWithRangeError
                , loadSuccessfully
                , load "()" "{ }" ()
                , load "(Bool, Bool)" "{ \"0\": false, \"1\": true }" (False, True)
                , load "(Bool, Bool)" "{ \"0\": false, \"1\": true }" (False, True)
                , load "Either Int Bool" "Left 2" (Left 2 :: Either Int Bool)
                , load "Either Int Bool" "Right true" (Right True :: Either Int Bool)
                , load "Either Int Bool" "Right true" (Right True :: Either Int Bool)
                , load "Int" "-2" (-2 :: Int)
                , load "Int8" "-2" (-2 :: Int8)
                , load "Int16" "-2" (-2 :: Int16)
                , load "Int32" "-2" (-2 :: Int32)
                , load "Int64" "-2" (-2 :: Int64)
                , load "Word" "2" (2 :: Word)
                , load "Word8" "2" (2 :: Word8)
                , load "Word16" "2" (2 :: Word16)
                , load "Word32" "2" (2 :: Word32)
                , load "Word64" "2" (2 :: Word64)
                , load "Natural" "2" (2 :: Natural)
                , load "Integer" "2" (2 :: Integer)
                , load "Integer" "+2" (2 :: Integer)
                , load "Scientific" "2" (2.0 :: Scientific)
                , load "Scientific" "+2" (2.0 :: Scientific)
                , load "Scientific" "2.5" (2.5 :: Scientific)
                , load "Double" "2.5" (2.5 :: Double)
                , load "Float" "2.5" (2.5 :: Float)
                , load "Text" "\"abc\"" ("abc" :: Text)
                , load "Lazy Text" "\"abc\"" ("abc" :: Text.Lazy.Text)
                , load "String" "\"abc\"" ("abc" :: String)
                , load "Key" "\"abc\"" ("abc" :: Key)
                , load "Value" "null" Aeson.Null
                , load "Seq Bool" "[ false, true ]" (Seq.fromList [ False, True ])
                , load "Vector Bool" "[ false, true ]" (Vector.fromList [ False, True ])
                , load "T0" "C0{ }" C0{ }
                , load "T0" "C1{ foo: \"abc\" }" C1{ foo = "abc" }
                , load "T0" "C2{ bar: 2 }" C2{ bar = 2, baz = Nothing }
                , load "T0" "C2{ bar: 3, baz: true }" C2{ bar = 3, baz = Just True }
                , load "T0" "C3{ }" C3{ a = Nothing, b = Nothing, c = Nothing }
                , load "T0" "C4{ }" C4{ a = Nothing, b = Nothing, c = Nothing, d = Nothing }
                ]

    let tests = Tasty.testGroup "Tests" [ autogeneratedTestTree, manualTestTree ]

    Tasty.defaultMain tests

interpretCode :: TestTree
interpretCode = Tasty.HUnit.testCase "interpret code" do
    actualValue <- throws (interpret (Code "(input)" "2 + 2"))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Natural }, Value.Scalar (Syntax.Natural 4))
          where
            location = Location{ name = "(input)", code = "2 + 2", offset = 2 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithImport :: TestTree
interpretCodeWithImport = Tasty.HUnit.testCase "interpret code with import from file" do
    actualValue <- throws (interpret (Code "(input)" "./tasty/data/unit/plus-input.ffg"))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Natural }, Value.Scalar (Syntax.Natural 5))
          where
            location = Location{ name = "./tasty/data/unit/plus-input.ffg", code = "2 + 3\n", offset = 2 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithEnvURI :: TestTree
interpretCodeWithEnvURI = Tasty.HUnit.testCase "interpret code with env: import" do
    let key = "GRACE_TEST_VAR"

    let name = "env:" <> key

    let open = do
            m <- Environment.lookupEnv key

            Environment.setEnv key "true"

            return m

    let close  Nothing  = Environment.unsetEnv key
        close (Just v ) = Environment.setEnv key v

    actualValue <- Exception.bracket open close \_ -> do
        throws (interpret (Code "(input)" (Text.pack name)))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Bool }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name, code = "true", offset = 0 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithFileURI :: TestTree
interpretCodeWithFileURI = Tasty.HUnit.testCase "interpret code with file:// import" do
    absolute <- Directory.makeAbsolute "./tasty/data/true.ffg"

    let uri = "file://" <> absolute

    actualValue <- throws (interpret (Code "(input)" (Text.pack uri)))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Bool }, Value.Scalar (Syntax.Bool True))
          where
            location = Location{ name = absolute, code = "true\n", offset = 0 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

loadSuccessfully :: TestTree
loadSuccessfully = Tasty.HUnit.testCase "load code" do
    let actual :: Either DecodingError Natural
        actual = decode (Value.Scalar (Syntax.Natural 2))

    Tasty.HUnit.assertEqual "" (Right 2) actual

load :: (Eq a, FromGrace a, Show a) => String -> Text -> a -> TestTree
load name code expected = Tasty.HUnit.testCase ("load " <> name) do
    actual <- Interpret.load (Code "(input)" code)

    Tasty.HUnit.assertEqual "" expected actual

data DecodingError = TypeError | RangeError deriving stock (Eq, Show)

decode :: FromGrace a => Value.Value -> Either DecodingError a
decode value = case Decode.decode value of
    Left  Decode.TypeError{ }  -> Left TypeError
    Left  Decode.RangeError{ } -> Left RangeError
    Right a                    -> Right a

decodeWithTypeError :: TestTree
decodeWithTypeError = Tasty.HUnit.testCase "load code with type error" do
    let actual₀ :: Either DecodingError Bool
        actual₀ = decode (Value.Scalar (Syntax.Natural 2))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₀

    let actual₁ :: Either DecodingError T0
        actual₁ = decode (Value.Alternative "C1" (Value.Record mempty))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₁

    let actual₂ :: Either DecodingError Natural
        actual₂ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₂

    let actual₃ :: Either DecodingError Integer
        actual₃ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₃

    let actual₄ :: Either DecodingError Text
        actual₄ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₄

    let actual₅ :: Either DecodingError Key
        actual₅ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₅

    let actual₆ :: Either DecodingError Value
        actual₆ = decode (Value.Lambda [] (Value.Name "x" Nothing) Syntax.Variable{ location = Unknown, name = "x" })

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₆

    let actual₇ :: Either DecodingError (Seq Bool)
        actual₇ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₇

    let actual₈ :: Either DecodingError Scientific
        actual₈ = decode (Value.Scalar (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₈

decodeWithRangeError :: TestTree
decodeWithRangeError = Tasty.HUnit.testCase "load code with range error" do
    let actual₀ :: Either DecodingError Word8
        actual₀ = decode (Value.Scalar (Syntax.Natural 256))

    Tasty.HUnit.assertEqual "" (Left RangeError) actual₀
