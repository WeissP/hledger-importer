import MyPrelude
import Test.Syd

main :: IO ()
main =
    sydTest
        $ describe "hledger-importer-test"
        $ it "works"
        $ 2
        + 2
        `shouldBe` (4 :: Int)
