{ mkDerivation, aeson, aeson-pretty, aeson-yaml, ansi-terminal
, async, base, bytestring, case-insensitive, containers, directory
, doctest, Earley, filepath, generic-lens, haskeline, http-client
, http-client-tls, http-types, insert-ordered-containers, lens, lib
, megaparsec, modern-uri, mtl, openai, optparse-applicative
, parser-combinators, prettyprinter, prettyprinter-ansi-terminal
, repline, retry, safe-exceptions, scientific, servant-client
, servant-client-core, string-interpolate, tasty, tasty-hunit
, tasty-silver, template-haskell, terminal-size, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "grace";
  version = "1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty aeson-yaml ansi-terminal async base bytestring
    case-insensitive containers Earley filepath generic-lens haskeline
    http-client http-client-tls http-types insert-ordered-containers
    lens megaparsec modern-uri mtl openai optparse-applicative
    parser-combinators prettyprinter prettyprinter-ansi-terminal
    repline retry safe-exceptions scientific servant-client
    servant-client-core string-interpolate template-haskell
    terminal-size text unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base directory doctest filepath mtl prettyprinter safe-exceptions
    tasty tasty-hunit tasty-silver text
  ];
  license = "unknown";
  mainProgram = "grace";
}
