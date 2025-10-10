-- | Utilities for interacting with GitHub
module Grace.GitHub
    ( -- * GitHub requests
      github
    , GitHub(..)
    ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace(..), Key(..))
import Grace.HTTP.Type (Header(..), HTTP(..), Parameter(..))

import qualified Data.Text as Text
import qualified Grace.Aeson
import qualified Grace.HTTP as HTTP

-- | An HTTP request to GitHub
data GitHub = GitHub
    { key :: Maybe Key
    , reference :: Maybe Text
    , owner :: Text
    , repository :: Text
    , path :: Text
    } deriving stock (Generic)
      deriving anyclass (FromGrace)

-- | Response from GitHub @\/repos/${owner}\/${repo}\/contents\/${path}@ API
data Contents = Contents{ download_url :: Text }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

{-| Make an HTTP request to GitHub

    This is an ergonomic convenience for the user for the exceedingly common use
    case of fetching code in version control from GitHub (and also powers
    trygrace.dev's `/github/${owner}/${repository}/${path}` short-links.
-}
github :: Bool -> GitHub -> IO (Text, Text)
github import_ GitHub{ key, owner, repository, reference, path } = do
    let authorization = case key of
            Nothing ->
                [ ]
            Just (Key k) ->
                [ Header{ header = "Authorization", value = "Bearer " <> Text.strip k } ]

    let headers = Just
            (   [ Header{ header = "X-GitHub-Api-Version", value = "2022-11-28" }
                , Header{ header = "User-Agent", value = "Grace" }
                ]
            <>  authorization
            )

    let parameters = do
            r <- reference

            return [ Parameter{ parameter = "ref", value = Just r } ]

    contentsResponse <- HTTP.http False GET
        { url = "https://api.github.com/repos/" <> owner <> "/" <> repository <> "/contents/" <> path
        , headers
        , parameters
        }

    Contents{ download_url } <- Grace.Aeson.decode contentsResponse

    contents <- HTTP.http import_ GET
        { url = download_url
        , headers = Nothing
        , parameters = Nothing
        }

    return (contents, download_url)
