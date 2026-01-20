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

import Control.Exception.Safe (Exception(..), SomeException(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (fold, toList)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Grace.Decode (FromGrace(..), Key(..), ToGraceType(..))
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Monad (Grace)
import Grace.Pretty (Pretty(..))
import Grace.Prompt.Types (Effort(..), Prompt(..))
import Grace.Type (Type(..))
import Grace.Value (Value)
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
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import qualified Grace.DataFile as DataFile
import qualified Grace.HTTP as HTTP
import qualified Grace.Import as Import
import qualified Grace.Infer as Infer
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty as Pretty
import qualified Grace.Prompt.Types as Prompt.Types
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified OpenAI.V1.Chat.Completions as Completions
import qualified Prettyprinter as Pretty
import qualified System.IO.Unsafe as Unsafe

deriving anyclass instance FromGrace ReasoningEffort
deriving anyclass instance ToGraceType ReasoningEffort

-- | Context used to teach the LLM to code in Grace
staticAssets :: [ Message (Vector Completions.Content) ]
staticAssets = Unsafe.unsafePerformIO do
    let instructions₀ =
            [ System
                { name = Just "Instructions"
                , content =
                    [ Completions.Text
                        { text =
                            "Here are some resources which explain how to program using the Fall-from-Grace programming language (\"Grace\" for short)"
                        }
                    ]
                }
            ]

    prompts <- do
        let files =
                [ "abnf.md"
                , "inference.md"
                ]

        let process :: FilePath -> IO (Message (Vector Completions.Content))
            process file = do
                text <- DataFile.readDataFile ("prompts" </> file)

                return System
                    { name = Just (Text.pack file)
                    , content = [ Completions.Text{ text } ]
                    }

        traverse process files

    let instructions₁ =
            [ System
                { name = Just "Instructions"
                , content =
                    [ Completions.Text
                        { text =
                            "Here are some sample Grace programs showcasing various idioms and language features"
                        }
                    ]
                }
            ]

    examples <- do
        let files =
                [ "learn-in-y-minutes.ffg"
                , "chaining.ffg"
                , "prompt.ffg"
                , "tools.ffg"
                ]

        let process :: FilePath -> IO (Message (Vector Completions.Content))
            process file = do
                text <- DataFile.readDataFile ("examples" </> file)

                return System
                    { name = Just (Text.pack file)
                    , content = [ Completions.Text{ text } ]
                    }

        traverse process files

    return (instructions₀ <> prompts <> instructions₁ <> examples)
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
                [ ("anyOf", Aeson.toJSON ([ present, absent ] :: [ Aeson.Value ]))
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

        let required = fmap fst fieldTypes

        return
            ( Aeson.object
                [ ("type", "object")
                , ("properties", Aeson.toJSON (Map.fromList properties))
                , ("additionalProperties", Aeson.toJSON False)
                , ("required", Aeson.toJSON required)
                ]
            )
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
    :: IO [(Text, Type Location, Value Location)]
    -> Bool
    -> Location
    -> Prompt
    -> Maybe (Type Location)
    -> Grace (Value Location)
prompt generateContext import_ location Prompt{ key = Grace.Decode.Key{ text = key }, text, history, model, search, effort } schema = do
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
            _ | defaultedSearch -> "gpt-5-search-api"
              | otherwise -> "gpt-5-mini"

    let reasoning_effort = do
            e <- effort

            return case e of
                Minimal -> ReasoningEffort_Minimal
                Low     -> ReasoningEffort_Low
                Medium  -> ReasoningEffort_Medium
                High    -> ReasoningEffort_High

    let toOutput ChatCompletionObject{ choices = [ Choice{ message = Assistant{ assistant_content = Just output } } ] } = do
            return output
        toOutput ChatCompletionObject{ choices } = do
            Exception.throwIO UnexpectedModelResponse{ choices }

    let lastMessage = case text of
            Nothing ->
                [ ]
            Just t ->
                [ User
                    { name = Nothing
                    , content =
                        [ Completions.Text{ text = t } ]
                    }
                ]

    let initMessages = do
            message <- fold history

            return case message of
                Prompt.Types.System{ name, text = t } -> System
                    { name
                    , content = [ Completions.Text{ text = t } ]
                    }
                Prompt.Types.Assistant{ name, text = t } -> Assistant
                    { name
                    , assistant_content = Just [ Completions.Text{ text = t } ]
                    , refusal = Nothing
                    , assistant_audio = Nothing
                    , tool_calls = Nothing
                    }
                Prompt.Types.User{ name, text = t } -> User
                    { name
                    , content = [ Completions.Text{ text = t } ]
                    }

    let conversation = initMessages <> lastMessage

    if import_
        then do
            let retry errors
                    | (_, interpretError) : rest <- errors
                    , length rest == 3 = do
                        Exception.throwIO interpretError
                    | otherwise = do
                        let instructions₀ = case conversation of
                                [ ] ->
                                    [ System
                                        { name = Just "Instructions"
                                        , content =
                                            [ Completions.Text
                                                { text = "Generate a Grace expression."
                                                }
                                            ]
                                        }
                                    ]

                                [ _ ] ->
                                    [ System
                                        { name = Just "Instructions"
                                        , content =
                                            [ Completions.Text
                                                { text = "Generate a Grace expression according to the previous message."
                                                }
                                            ]
                                        }
                                    ]

                                _ ->
                                    [ System
                                        { name = Just "Instructions"
                                        , content =
                                            [ Completions.Text
                                                { text = "Generate a Grace expression according to the previous conversation."
                                                }
                                            ]
                                        }
                                    ]

                        let expect = case schema of
                                Nothing ->
                                    [ ]
                                Just s ->
                                    [ System
                                        { name = Just "Instructions"
                                        , content =
                                            [ Completions.Text
                                                { text = "Your generated Grace expression must have the following type"
                                                }
                                            ]
                                        }
                                    , System
                                        { name = Just "Type"
                                        , content =
                                            [ Completions.Text
                                                { text = Pretty.toSmart s
                                                }
                                            ]
                                        }
                                    ]

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

                        let environment = do
                                binding <- bindings

                                return System
                                    { name = Just "Value"
                                    , content =
                                        [ Completions.Text
                                            { text = renderAssignment binding
                                            }
                                        ]
                                    }

                        let instructions₁ = case environment of
                                [ ] ->
                                    [ ]
                                _ ->
                                    [ System
                                        { name = Just "Instructions"
                                        , content =
                                            [ Completions.Text
                                                { text = "You may use any of the following values to generate your Grace expression, all of which are in scope."
                                                }
                                            ]
                                        }
                                    ]

                        let instructions₂ =
                                [ System
                                    { name = Just "Instructions"
                                    , content =
                                        [ Completions.Text
                                            { text = "Output a naked Grace expression without any code fence or explanation.  Your response in its entirety should be a valid input to the Grace interpreter."
                                            }
                                        ]
                                    }
                                ]

                        let failedAttempts = do
                                (program, interpretError) <- reverse errors

                                let attempt = Assistant
                                        { name = Just "Attempt"
                                        , assistant_content = Just [ Completions.Text{ text = program } ]
                                        , refusal = Nothing
                                        , assistant_audio = Nothing
                                        , tool_calls = Nothing
                                        }

                                let failure = System
                                        { name = Just "Error"
                                        , content =
                                            [ Completions.Text
                                                { text = Text.pack (displayException interpretError) }
                                            ]
                                        }

                                [ attempt, failure ]

                        let messages =
                                Vector.fromList
                                    (   staticAssets
                                    <>  conversation
                                    <>  instructions₀
                                    <>  expect
                                    <>  instructions₁
                                    <>  environment
                                    <>  instructions₂
                                    <>  failedAttempts
                                    )

                        chatCompletionObject <- liftIO do
                            HTTP.createChatCompletion methods _CreateChatCompletion
                                { messages
                                , model = Model defaultedModel
                                , web_search_options
                                , reasoning_effort
                                }

                        output <- toOutput chatCompletionObject

                        parent <- Reader.ask

                        Reader.local (\i -> i <> Code "(prompt)" output) do
                            child <- Reader.ask

                            Import.referentiallySane parent child

                            Interpret.interpretWith bindings schema
                                `Exception.catch` \(interpretError :: SomeException) -> do
                                    retry ((output, interpretError) : errors)


            (_, e) <- retry []

            return e

        else do
            let instructions₀ = case conversation of
                    [ ] ->
                        [ System
                            { name = Just "Instructions"
                            , content =
                                [ Completions.Text
                                    { text = "Generate JSON."
                                    }
                                ]
                            }
                        ]

                    [ _ ] ->
                        [ System
                            { name = Just "Instructions"
                            , content =
                                [ Completions.Text
                                    { text = "Generate JSON according to the previous message."
                                    }
                                ]
                            }
                        ]

                    _ ->
                        [ System
                            { name = Just "Instructions"
                            , content =
                                [ Completions.Text
                                    { text = "Generate JSON according to the previous conversation."
                                    }
                                ]
                            }
                        ]

            let defaultedSchema = do
                    s <- schema

                    return (Type.defaultTo Type.Scalar{ scalar = Monotype.Text, location } s)

            let decode_ text_ = do
                    let bytes = Encoding.encodeUtf8 text_

                    let lazyBytes = ByteString.Lazy.fromStrict bytes

                    case Aeson.eitherDecode lazyBytes of
                        Left message_ -> Exception.throwIO ModelDecodingFailed{ message = message_, text = text_ }
                        Right v -> return v

            let expect = case schema of
                    Nothing ->
                        [ ]
                    Just s ->
                        [ System
                            { name = Just "Instructions"
                            , content =
                                [ Completions.Text
                                    { text = "Your generated JSON must have the following type"
                                    }
                                ]
                            }
                        , System
                            { name = Just "Type"
                            , content =
                                [ Completions.Text
                                    { text = Pretty.toSmart s
                                    }
                                ]
                            }
                        ]

            let instructions₁ = instructions₀ <> expect

            let extractText = do
                    let extract text_ = do
                            return (Value.Text Unknown text_)

                    return
                        ( [ ]
                        , ResponseFormat_Text
                        , extract
                        )

            let extractRecord = do
                    responseFormat <- case toResponseFormat defaultedSchema of
                        Left exception -> Exception.throwIO exception
                        Right result -> return result

                    let extract text_ = do
                            v <- decode_ text_

                            value <- case defaultedSchema of
                                Nothing -> do
                                    return (Infer.inferJSON v)
                                Just s -> do
                                    Infer.checkJSON s v

                            return (fmap (\_ -> Unknown) value)

                    return
                        ( instructions₁
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
                                    return (Infer.inferJSON v)
                                Just s -> do
                                    Infer.checkJSON s v

                            case fmap (\_ -> Unknown) expression of
                                Value.Record _ [("response", (_, response))] -> do
                                    return response
                                other -> do
                                    return other

                    return
                        ( instructions₁
                        , responseFormat
                        , extract
                        )

            (instructions₂, response_format, extract) <- case defaultedSchema of
                Just Type.Scalar{ scalar = Monotype.Text } -> extractText
                Just Type.Record{ } -> extractRecord
                _ -> extractNonRecord

            let messages₀ = Vector.fromList (conversation <> instructions₂)

            let messages₁ = case messages₀ of
                    [ ] ->
                        [ User
                            { name = Nothing
                            , content = [ Completions.Text{ text = "" } ]
                            }
                        ]

                    _ -> messages₀

            chatCompletionObject <- liftIO do
                HTTP.createChatCompletion methods _CreateChatCompletion
                    { messages = messages₁
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
