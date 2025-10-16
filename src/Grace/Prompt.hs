{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the @prompt@ keyword
module Grace.Prompt
    ( -- * Prompting
      Prompt(..)
    , Effort(..)
    , prompt

      -- * Exceptions
    , UnsupportedModelOutput(..)
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (Exception(..), MonadCatch, SomeException(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Grace.Decode (FromGrace(..), Key(..))
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Prompt.Types (Effort(..), Prompt(..))
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

import Grace.Infer (Status(..))
import {-# SOURCE #-} qualified Grace.Interpret as Interpret

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Grace.Context as Context
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

toResponseFormat
    :: Maybe (Type a) -> Either (UnsupportedModelOutput a) ResponseFormat
toResponseFormat Nothing = do
    return JSON_Object
toResponseFormat (Just type_) = do
    value <- toJSONSchema type_

    return JSON_Schema
        { json_schema = JSONSchema
            { description = Nothing
            , name = "result"
            , schema = Just value
            , strict = Just True
            }
        }

-- | Implementation of the @prompt@ keyword
prompt
    :: (MonadCatch m, MonadState Status m, MonadIO m)
    => IO [(Text, Type Location, Value)]
    -> Bool
    -> Location
    -> Prompt
    -> Maybe (Type Location)
    -> m Value
prompt generateContext import_ location Prompt{ key = Grace.Decode.Key{ text = key }, text, model, search, effort } schema = do
    keyToMethods <- liftIO HTTP.getMethods

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
              | otherwise -> "gpt-5-mini"

    let reasoning_effort = do
            e <- effort

            return case e of
                Low    -> ReasoningEffort_Low
                Medium -> ReasoningEffort_Medium
                High   -> ReasoningEffort_High

    let toOutput ChatCompletionObject{ choices = [ Choice{ message = Assistant{ assistant_content = Just output } } ] } = do
            return output
        toOutput ChatCompletionObject{ choices } = do
            Exception.throwIO UnexpectedModelResponse{ choices }

    if import_
        then do
            let retry errors
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

                        bindings <- liftIO generateContext

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
                                | Foldable.null bindings = ""
                                | otherwise =
                                    "Given the following variables:\n\
                                    \\n\
                                    \" <> foldMap renderAssignment bindings

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
                                \" <> environment <> expect <> instructions <> "\
                                \Output a naked Grace expression without any code fence or explanation.\n\
                                \Your response in its entirety should be a valid input to the Grace interpreter.\n\
                                \\n\
                                \" <> Text.concat failedAttempts
                              where
                                expect = case schema of
                                    Just s ->
                                        "Generate a standalone Grace expression matching the following type:\n\
                                        \\n\
                                        \" <> Pretty.toSmart s <> "\n\
                                        \\n"
                                    Nothing ->
                                        ""


                        chatCompletionObject <- liftIO do
                            HTTP.createChatCompletion methods _CreateChatCompletion
                                { messages = [ User{ content = [ Completions.Text{ text = input } ], name = Nothing } ]
                                , model = Model defaultedModel
                                , web_search_options
                                , reasoning_effort
                                }

                        output <- toOutput chatCompletionObject

                        Status{ input = parent } <- State.get

                        let child = parent <> Code "(prompt)" output

                        State.modify (\s -> (s :: Status){ input = child })

                        expression <- Interpret.interpretWith keyToMethods bindings schema
                            `Exception.catch` \(interpretError :: SomeException) -> do
                                retry ((output, interpretError) : errors)

                        State.modify (\s -> (s :: Status){ input = parent })

                        return expression

            (_, e) <- retry []

            return e
        else do
            Status{ context } <- State.get

            let defaultedSchema = do
                    s <- schema

                    return (Lens.transform (Type.defaultTo Type.Scalar{ scalar = Monotype.Text, location }) (Context.complete context s))

            let decode_ text_ = do
                    let bytes = Encoding.encodeUtf8 text_

                    let lazyBytes = ByteString.Lazy.fromStrict bytes

                    case Aeson.eitherDecode lazyBytes of
                        Left message_ -> Exception.throwIO ModelDecodingFailed{ message = message_, text = text_ }
                        Right v -> return v

            let requestJSON =
                    instructions <> jsonSchema
                  where
                    instructions = case text of
                        Nothing ->
                            ""
                        Just p ->
                            p <> "\n\

                            \\n"
                    jsonSchema = case defaultedSchema of
                        Nothing ->
                            "Generate JSON output"
                        Just s  ->
                            "Generate JSON output matching the following type:\n\
                            \\n\
                            \" <> Pretty.toSmart s


            let extractText = do
                    let extract text_ = do
                            return (Value.Text text_)

                    let instructions = case text of
                            Nothing -> ""
                            Just p  -> p

                    return
                        ( instructions
                        , ResponseFormat_Text
                        , extract
                        )

            let extractRecord = do
                    responseFormat <- case toResponseFormat defaultedSchema of
                        Left exception -> Exception.throwIO exception
                        Right result -> return result

                    let extract text_ = do
                            v <- decode_ text_

                            case defaultedSchema of
                                Nothing -> do
                                    return (Value.inferJSON v)
                                Just s -> do
                                    case Value.checkJSON s v of
                                        Left invalidJSON -> Exception.throwIO invalidJSON
                                        Right e -> return e

                    return
                        ( requestJSON
                        , responseFormat
                        , extract
                        )

            let extractNonRecord = do
                    let adjustedSchema = do
                            s <- defaultedSchema

                            return (Type.Record (Type.location s) (Type.Fields [("response", s)] Monotype.EmptyFields))

                    responseFormat <- case toResponseFormat adjustedSchema of
                        Left exception -> Exception.throwIO exception
                        Right result -> return result

                    let extract text_ = do
                            v <- decode_ text_

                            expression <- case adjustedSchema of
                                Nothing -> do
                                    return (Value.inferJSON v)
                                Just s -> do
                                    case Value.checkJSON s v of
                                        Left invalidJSON -> Exception.throwIO invalidJSON
                                        Right expression -> return expression

                            case expression of
                                Value.Record [("response", response)] -> do
                                    return response
                                other -> do
                                    return other

                    return
                        ( requestJSON
                        , responseFormat
                        , extract
                        )

            (text_, response_format, extract) <- case defaultedSchema of
                Just Type.Scalar{ scalar = Monotype.Text } -> extractText
                Just Type.Record{ } -> extractRecord
                _ -> extractNonRecord

            chatCompletionObject <- liftIO do
                HTTP.createChatCompletion methods _CreateChatCompletion
                    { messages = [ User{ content = [ Completions.Text{ text = text_ } ], name = Nothing  } ]
                    , model = Model defaultedModel
                    , response_format = Just response_format
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
