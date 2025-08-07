{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the @prompt@ keyword
module Grace.Prompt
    ( Prompt(..)
    , prompt

      -- * Exceptions
    , UnsupportedModelOutput(..)
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (Exception(..), SomeException(..))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace(..), Key(..))
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Type (Type(..))
import Grace.Value (Value)
import Numeric.Natural (Natural)
import OpenAI.V1.Models (Model(..))
import OpenAI.V1.ResponseFormat (JSONSchema(..), ResponseFormat(..))
import System.FilePath ((</>))

import OpenAI.V1.Chat.Completions
    ( ChatCompletionObject(..)
    , Choice(..)
    , CreateChatCompletion(..)
    , Message(..)
    , ReasoningEffort(..)
    , WebSearchOptions(..)
    , _CreateChatCompletion
    )

import {-# SOURCE #-} qualified Grace.Interpret as Interpret

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Grace.DataFile as DataFile
import qualified Grace.HTTP as HTTP
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified OpenAI.V1.Chat.Completions as Completions
import qualified Prettyprinter as Pretty
import qualified System.IO.Unsafe as Unsafe

deriving anyclass instance FromGrace ReasoningEffort

-- | The amount of effort a reasoning model puts into reasoning
data Effort = Low | Medium | High
    deriving stock (Generic)
    deriving anyclass (FromGrace)

fromEffort :: Effort -> ReasoningEffort
fromEffort Low = ReasoningEffort_Low
fromEffort Medium = ReasoningEffort_Medium
fromEffort High = ReasoningEffort_High

-- | Context used to teach the LLM to code in Grace
staticAssets :: Text
staticAssets = Unsafe.unsafePerformIO do
    examples <- do
        let files :: [FilePath]
            files =
                [ "learn-in-y-minutes.ffg"
                , "chaining.ffg"
                , "prompt.ffg"
                , "tools.ffg"
                ]

        let process file = do
                content <- DataFile.readDataFile ("examples" </> file)

                return
                    ( "Example: " <> Text.pack file <> "\n\
                      \\n\
                      \" <> content <> "\n\
                      \\n"
                    )

        traverse process files

    prompts <- do
        let files :: [FilePath]
            files =
                [ "inference.md"
                , "abnf.md"
                ]

        let process file = do
                content <- DataFile.readDataFile ("prompts" </> file)

                return
                    ( "Post: " <> Text.pack file <> "\n\
                      \\n\
                      \" <> content <> "\n\
                      \\n"
                    )

        traverse process files

    return (Text.concat prompts <> "\n\n" <> Text.concat examples)
{-# NOINLINE staticAssets #-}

toJSONSchema :: Type a -> Either (UnsupportedModelOutput a) Aeson.Value
toJSONSchema original = loop original
  where
    loop Type.Forall{ location, name, type_ } = do
        loop
            (Type.substituteType name 0 Type.Scalar{ location, scalar = Monotype.Text } type_)
    loop Type.Optional{ type_ } = do
        present <- loop type_

        let absent = Aeson.object [ ("type", "null") ]

        return
            ( Aeson.object
                [ ("type", "object")
                , ("anyOf", Aeson.toJSON ([ present, absent ] :: [ Aeson.Value ]))
                ]
            )

    loop Type.List{ type_ } = do
        items <- loop type_

        return (Aeson.object [ ("type", "array"), ("items", items) ])
    loop Type.Record{ fields = Type.Fields fieldTypes _ } = do
        let toProperty (field, type_) = do
                property <- loop type_

                return (field, property)

        properties <- traverse toProperty fieldTypes

        return
            ( Aeson.object
                [ ("type", "object")
                , ("properties", Aeson.toJSON (Map.fromList properties))
                , ("additionalProperties", Aeson.toJSON False)
                , ("required", Aeson.toJSON required)
                ]
            )
      where
        required = do
            (field, type_) <- fieldTypes

            case type_ of
                Type.Optional{ } -> empty
                _ -> return field
    loop Type.Union{ alternatives = Type.Alternatives alternativeTypes _ } = do
        let toAnyOf (alternative, type_) = do
                contents <- loop type_

                return
                    (Aeson.object
                        [ ("type", "object")
                        , ( "properties"
                          , Aeson.object
                              [ ( "tag"
                                , Aeson.object
                                    [ ("type", "string")
                                    , ("const", Aeson.toJSON alternative)
                                    ]
                                )
                              , ("contents", contents)
                              ]
                          )
                        , ("required", Aeson.toJSON ([ "tag", "contents" ] :: [Text]))
                        , ("additionalProperties", Aeson.toJSON False)
                        ]
                    )

        anyOfs <- traverse toAnyOf alternativeTypes

        return
            ( Aeson.object
                [ ("type", "object"), ("anyOf", Aeson.toJSON anyOfs) ]
            )
    loop Type.Scalar{ scalar = Monotype.Bool } =
        return (Aeson.object [ ("type", "boolean") ])
    loop Type.Scalar{ scalar = Monotype.Real } =
        return (Aeson.object [ ("type", "number") ])
    loop Type.Scalar{ scalar = Monotype.Integer } =
        return (Aeson.object [ ("type", "integer") ])
    loop Type.Scalar{ scalar = Monotype.JSON } =
        return (Aeson.object [ ])
    loop Type.Scalar{ scalar = Monotype.Natural } =
        return
            (Aeson.object
                [ ("type", "number")
                -- , ("minimum", Aeson.toJSON (0 :: Int))
                -- ^ Not supported by OpenAI
                ]
            )
    loop Type.Scalar{ scalar = Monotype.Text } =
        return (Aeson.object [ ("type", "string") ])
    loop _ = Left UnsupportedModelOutput{ original }

-- | Arguments to the @prompt@ keyword
data Prompt = Prompt
    { key :: Grace.Decode.Key
    , text :: Maybe Text
    , model :: Maybe Text
    , code :: Maybe Bool
    , search :: Maybe Bool
    , effort :: Maybe Effort
    } deriving stock (Generic)
      deriving anyclass (FromGrace)

-- | Implementation of the @prompt@ keyword
prompt
    :: IO [(Text, Type Location, Value)]
    -> Location
    -> Prompt
    -> Type Location
    -> IO Value
prompt generateContext location Prompt{ key = Grace.Decode.Key{ text = key }, text, model, code, search, effort } schema = do
    keyToMethods <- HTTP.getMethods

    let methods = keyToMethods (Text.strip key)

    let defaultedSearch = case search of
            Just s -> s
            Nothing -> False

    let web_search_options
            | defaultedSearch = Just WebSearchOptions
                { search_context_size = Nothing
                , user_location = Nothing
                }
            | otherwise = Nothing

    let defaultedModel = case model of
            Just m -> m
            _ | defaultedSearch -> "gpt-4o-search-preview"
              | otherwise -> "o4-mini"

    let defaultedCode = case code of
            Just c -> c
            Nothing -> False

    let reasoning_effort = case effort of
            Nothing
                | Text.isPrefixOf "o" defaultedModel ->
                    Just ReasoningEffort_High
            _ ->
                fmap fromEffort effort

    let defaultedSchema =
            Lens.transform
                (Type.defaultTo Type.Scalar{ scalar = Monotype.Text, location })
                schema

    let toResponseFormat s = JSON_Schema
            { json_schema = JSONSchema
                { description = Nothing
                , name = "result"
                , schema = Just s
                , strict = Just True
                }
            }

    let toOutput ChatCompletionObject{ choices = [ Choice{ message = Assistant{ assistant_content = Just output } } ] } = do
            return output
        toOutput ChatCompletionObject{ choices } = do
            Exception.throwIO UnexpectedModelResponse{ choices }

    if defaultedCode
        then do
            let retry :: [(Text, SomeException)] -> IO (Type Location, Value)
                retry errors
                    | (_, interpretError) : rest <- errors
                    , length rest == 3 = do
                        Exception.throwIO interpretError
                    | otherwise = do
                        let failedAttempts = do
                                (index, (program, interpretError)) <- zip [ 0 .. ] (reverse errors)
                                return
                                    ( "Your failed attempt " <> Text.pack (show (index :: Natural)) <> ":\n\
                                      \\n\
                                      \" <> program <> "\n\
                                      \\n\
                                      \Error:\n\
                                      \" <> Text.pack (displayException interpretError) <> "\n\
                                      \\n"
                                    )

                        context <- generateContext

                        let renderAssignment (name, type_, _) =
                                Pretty.toSmart (Pretty.group (Pretty.flatAlt long short)) <> "\n\n"
                              where
                                long =  Pretty.label (pretty name)
                                    <>  " "
                                    <>  Pretty.punctuation ":"
                                    <>  Pretty.hardline
                                    <>  "  "
                                    <>  Pretty.nest 2 (pretty type_)

                                short = Pretty.label (pretty name)
                                    <>  " "
                                    <>  Pretty.punctuation ":"
                                    <>  " "
                                    <>  pretty type_

                        let environment
                                | Foldable.null context = ""
                                | otherwise =
                                    "Given the following variables:\n\
                                    \\n\
                                    \" <> foldMap renderAssignment context

                        let instructions = case text of
                                Nothing ->
                                    ""
                                Just p ->
                                    "… according to these instructions:\n\
                                    \\n\
                                    \" <> p <> "\n\
                                    \\n"

                        let input =
                                staticAssets <> "\n\
                                \\n\
                                \" <> environment <> "\
                                \Generate a standalone Grace expression matching the following type:\n\
                                \\n\
                                \" <> Pretty.toSmart schema <> "\n\
                                \\n\
                                \" <> instructions <> "\
                                \Output a naked Grace expression without any code fence or explanation.\n\
                                \Your response in its entirety should be a valid input to the Grace interpreter.\n\
                                \\n\
                                \" <> Text.concat failedAttempts

                        chatCompletionObject <- do
                            HTTP.createChatCompletion methods _CreateChatCompletion
                                { messages = [ User{ content = [ Completions.Text{ text = input } ], name = Nothing } ]
                                , model = Model defaultedModel
                                , web_search_options
                                , reasoning_effort
                                }

                        output <- toOutput chatCompletionObject

                        Interpret.interpretWith keyToMethods context (Just schema) (Code "(generated)" output)
                            `Exception.catch` \interpretError -> do
                                retry ((output, interpretError) : errors)

            (_, e) <- retry []

            return e
        else do
            let decode_ text_ = do
                    let bytes = Encoding.encodeUtf8 text_

                    let lazyBytes = ByteString.Lazy.fromStrict bytes

                    case Aeson.eitherDecode lazyBytes of
                        Left message_ -> Exception.throwIO ModelDecodingFailed{ message = message_, text = text_ }
                        Right v -> return v

            let requestJSON =
                    instructions <> "Generate JSON output matching the following type:\n\
                    \\n\
                    \" <> Pretty.toSmart defaultedSchema
                  where
                    instructions = case text of
                        Nothing ->
                            ""
                        Just p ->
                            p <> "\n\
                            \\n"

            let extractText = do
                    let extract text_ = do
                            return (Value.Text text_)

                    let instructions = case text of
                            Nothing -> ""
                            Just p  -> p

                    return
                        ( instructions
                        , Nothing
                        , extract
                        )

            let extractRecord = do
                    jsonSchema <- case toJSONSchema defaultedSchema of
                        Left exception -> Exception.throwIO exception
                        Right result -> return result

                    let extract text_ = do
                            v <- decode_ text_

                            case Value.fromJSON defaultedSchema v of
                                Left invalidJSON -> Exception.throwIO invalidJSON
                                Right e -> return e

                    return
                        ( requestJSON
                        , Just (toResponseFormat jsonSchema)
                        , extract
                        )

            let extractNonRecord = do
                    let adjustedSchema =
                            Type.Record (Type.location defaultedSchema) (Type.Fields [("response", defaultedSchema)] Monotype.EmptyFields)

                    jsonSchema <- case toJSONSchema adjustedSchema of
                        Left exception -> Exception.throwIO exception
                        Right result -> return result

                    let extract text_ = do
                            v <- decode_ text_

                            expression <- case Value.fromJSON adjustedSchema v of
                                Left invalidJSON -> Exception.throwIO invalidJSON
                                Right expression -> return expression

                            case expression of
                                Value.Record [("response", response)] -> do
                                    return response
                                other -> do
                                    return other

                    return
                        ( requestJSON
                        , Just (toResponseFormat jsonSchema)
                        , extract
                        )

            (text_, response_format, extract) <- case defaultedSchema of
                    Type.Scalar{ scalar = Monotype.Text } -> extractText
                    Type.Record{ } -> extractRecord
                    _ -> extractNonRecord

            chatCompletionObject <- do
                HTTP.createChatCompletion methods _CreateChatCompletion
                    { messages = [ User{ content = [ Completions.Text{ text = text_ } ], name = Nothing  } ]
                    , model = Model defaultedModel
                    , response_format
                    , reasoning_effort
                    }

            output <- toOutput chatCompletionObject

            extract output

-- | The expected type for the model output can't be encoded as JSON
newtype UnsupportedModelOutput a = UnsupportedModelOutput{ original :: Type a }
    deriving stock (Show)

instance (Show a, Typeable a) => Exception (UnsupportedModelOutput a) where
    displayException UnsupportedModelOutput{ original } =
        "Unsupported model output type\n\
        \\n\
        \The expected type for the model output is:\n\
        \\n\
        \" <> Text.unpack (Pretty.toSmart original) <> "\n\
        \\n\
        \… but that type cannot be encoded as JSON"

-- | The model didn't return an expected, successful response
data UnexpectedModelResponse = UnexpectedModelResponse{ choices :: Vector Choice }
    deriving stock (Show)

instance Exception UnexpectedModelResponse where
    displayException UnexpectedModelResponse{ choices } =
        case toList choices of
            [] ->
                "Unexpected model response\n\
                \\n\
                \The model did not return any choices"

            _ : _ : _ ->
                "Unexpected model response\n\
                \\n\
                \The model returned multiple choices when only one was expected"
            [ Choice{ message = Assistant{ refusal = Just refusal } } ] ->
                "Unexpected model response\n\
                \\n\
                \The model refused to answer for the following reason:\n\
                \\n\
                \" <> Text.unpack refusal
            [ Choice{ message = Assistant{ assistant_content = Nothing } } ] ->
                "Unexpected model response\n\
                \\n\
                \The model returned an empty answer"
            [ Choice{ message } ] ->
                "Unexpected model response\n\
                \\n\
                \The model responded with a non-assistant message\n\
                \\n\
                \Message:\n\
                \\n\
                \" <> string
              where
                bytes = ByteString.Lazy.toStrict (Aeson.encode message)

                string = case Encoding.decodeUtf8' bytes of
                    Left  _    -> show bytes
                    Right text -> Text.unpack text

-- | Model decoding failed
data ModelDecodingFailed = ModelDecodingFailed
    { message :: String
    , text :: Text
    } deriving stock (Show)

instance Exception ModelDecodingFailed where
    displayException ModelDecodingFailed{ message, text } =
        "Failed to decode output as JSON\n\
        \\n\
        \The model produced the following output:\n\
        \\n\
        \" <> Text.unpack text <> "\n\
        \\n\
        \… which failed to decode as JSON.\n\
        \\n\
        \Decoding error message:\n\
        \\n\
        \" <> message
