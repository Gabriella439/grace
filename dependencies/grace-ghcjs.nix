{ mkDerivation, aeson, aeson-pretty, aeson-yaml, async, base
, binary, bytestring, case-insensitive, containers, Earley
, filepath, generic-lens, ghcjs-base, ghcjs-fetch, ghcjs-prim
, http-types, insert-ordered-containers, lens, lib, megaparsec
, modern-uri, mtl, openai, parser-combinators, prettyprinter
, prettyprinter-ansi-terminal, safe-exceptions, scientific, stm
, string-interpolate, template-haskell, text, transformers
, unordered-containers, uri-encode, vector
}:
mkDerivation {
  pname = "grace";
  version = "1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty aeson-yaml async base binary bytestring
    case-insensitive containers Earley filepath generic-lens ghcjs-base
    ghcjs-fetch ghcjs-prim http-types insert-ordered-containers lens
    megaparsec modern-uri mtl openai parser-combinators prettyprinter
    prettyprinter-ansi-terminal safe-exceptions scientific
    string-interpolate template-haskell text unordered-containers
    vector
  ];
  executableHaskellDepends = [
    aeson async base containers filepath ghcjs-base
    insert-ordered-containers lens mtl safe-exceptions scientific stm
    text transformers uri-encode
  ];
  doHaddock = false;
  doCheck = false;
  license = "unknown";
  mainProgram = "try-grace";
}
