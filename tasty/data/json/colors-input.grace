# https://www.sitepoint.com/colors-json-example/
#
# Note that the example from the above page is not valid JSON (due to a
# trailing comma)
./colors.json :
  { colors: List
    (exists (a : Fields) .
      { color: Text
      , category: Text
      , code: { rgba: List Natural, hex: Text }
      , a
      }
    )
  }
