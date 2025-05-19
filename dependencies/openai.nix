{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, filepath, http-api-data, http-client, http-client-tls, lib
, servant, servant-client, servant-multipart-api
, servant-multipart-client, tasty, tasty-hunit, text, time, vector
}:
mkDerivation {
  pname = "openai";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/openai.git";
    sha256 = "1z25h5nnsm5x6lbqmz58k4va3hymab53d7rj2apm5yv7058y5zys";
    rev = "681e8f6e9802764ddb64ea428ffd79035b00c721";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers filepath http-api-data http-client
    http-client-tls servant servant-client servant-multipart-api
    servant-multipart-client text time vector
  ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    aeson base http-client http-client-tls servant-client tasty
    tasty-hunit text
  ];
  doCheck = false;
  description = "Servant bindings to OpenAI";
  license = lib.licenses.bsd3;
  mainProgram = "openai-example";
}
