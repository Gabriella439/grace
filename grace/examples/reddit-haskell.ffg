# This is an example of extracting post titles from a JSON input downloaded from
# https://reddit.com/r/haskell.json
let input
      = ./reddit-haskell.json
      : { data: { children: List { data: { title: Text, ? }, ? }, ? }, ? }

in  List/map (\child -> child.data.title) input.data.children
