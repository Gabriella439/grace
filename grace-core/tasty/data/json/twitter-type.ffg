forall (a : Type) .
forall (b : Type) .
forall (c : Type) .
  List
    { created_at: Text
    , id: Natural
    , id_str: Text
    , text: Text
    , truncated: Bool
    , entities: { hashtags: List { text: Text, indices: List Natural }
                , symbols: List a
                , user_mentions: List b
                , urls: List
                          { url: Text
                          , expanded_url: Text
                          , display_url: Text
                          , indices: List Natural
                          }
                }
    , source: Text
    , user: { id: Natural
            , id_str: Text
            , name: Text
            , screen_name: Text
            , location: Text
            , description: Text
            , url: Text
            , entities: { url: { urls: List
                                         { url: Text
                                         , expanded_url: Text
                                         , display_url: Text
                                         , indices: List Natural
                                         }
                               }
                        , description: { urls: List c }
                        }
            , protected: Bool
            , followers_count: Natural
            , friends_count: Natural
            , listed_count: Natural
            , created_at: Text
            , favourites_count: Natural
            , utc_offset: Natural
            , time_zone: Text
            }
    }
