# Grace version of an example from https://dhall-lang.org/
let makeUser = \user ->
      let home       = "/home/" + user

      let privateKey = home + "/.ssh/id_ed25519"

      let publicKey  = privateKey + ".pub"

      in  { home: home, privateKey: privateKey, publicKey: publicKey }

in  [ makeUser "bill"
    , makeUser "jane"
    ]
