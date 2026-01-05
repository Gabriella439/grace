{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, filepath, http-api-data, http-client, http-client-tls, http-types
, lib, servant, servant-client, servant-multipart-api
, servant-multipart-client, tasty, tasty-hunit, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "openai";
  version = "2.2.1";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/openai.git";
    sha256 = "019f13b5yfsqbzaspkazqlwx5f27iq5ns2r1irr9p86plc6sfyhy";
    rev = "e3a5d8512a27842f080e608b00442c1096942d09";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers filepath http-api-data http-client
    http-client-tls http-types servant servant-client
    servant-multipart-api servant-multipart-client text time
    unordered-containers vector
  ];
  executableHaskellDepends = [ aeson base bytestring text vector ];
  testHaskellDepends = [
    aeson base http-client http-client-tls servant-client tasty
    tasty-hunit text
  ];
  description = "Servant bindings to OpenAI";
  license = lib.licenses.bsd3;
}
