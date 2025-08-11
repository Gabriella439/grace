{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, filepath, http-api-data, http-client, http-client-tls, lib
, servant, servant-client, servant-multipart-api
, servant-multipart-client, tasty, tasty-hunit, text, time, vector
}:
mkDerivation {
  pname = "openai";
  version = "1.1.1";
  src = fetchgit {
    url = "https://github.com/MercuryTechnologies/openai.git";
    sha256 = "1aqbs462jgq38qysywwnj8y5krj99lr3cmisar2d12b9iabzwsxm";
    rev = "f6cf4baa60a4a21a489d60247c47bc57079524a2";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers filepath http-api-data http-client
    http-client-tls servant servant-client servant-multipart-api
    servant-multipart-client text time vector
  ];
  executableHaskellDepends = [ aeson base bytestring text vector ];
  testHaskellDepends = [
    aeson base http-client http-client-tls servant-client tasty
    tasty-hunit text
  ];
  doCheck = false;
  description = "Servant bindings to OpenAI";
  license = lib.licenses.bsd3;
}
