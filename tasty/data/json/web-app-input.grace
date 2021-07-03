./web-app.json :
  { web-app:
    { servlet: List (exists (a : Type) . a )
    , servlet-mapping:
      { cofaxCDS: Text
      , cofaxEmail: Text
      , cofaxAdmin: Text
      , fileServlet: Text
      , cofaxTools: Text
      }
    , taglib:
      { taglib-uri: Text
      , taglib-location: Text
      }
    }
  }
