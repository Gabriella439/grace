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
    sha256 = "1wfgbsifz1svyyb3x6zlfvakc2ydzz3gdh3xlw2fbcrb3kz492qs";
    rev = "cf8b361762529ec7f1b3326bf238293c9a4ae824";
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
