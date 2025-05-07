import qualified Test.DocTest

main :: IO ()
main = Test.DocTest.doctest [ "-idist/build/autogen", "--fast", "src", "ghc" ]
