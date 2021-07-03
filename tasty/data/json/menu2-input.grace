./menu2.json :
  { menu:
    { header: Text
    , items: List (Optional (exists (a : Fields) . { id: Text, a }))
    }
  }
