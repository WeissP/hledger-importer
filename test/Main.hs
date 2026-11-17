import Catch (match)
import MyPrelude
import Test.Syd
import Types

main :: IO ()
main = sydTest $ describe "Catch.match" do
  describe "IContains" do
    let m = Matcher IContains
    it "matches regardless of ASCII case" do
      match "Payment to AMAZON EU" (m "amazon") `shouldBe` True
      match "payment to amazon eu" (m "AMAZON") `shouldBe` True
    it "matches regardless of non-ASCII case" do
      match "ÜBERWEISUNG Müller" (m "überweisung müller") `shouldBe` True
    it "does not match absent substrings" do
      match "Payment to Amazon" (m "ebay") `shouldBe` False
  describe "Contains" do
    it "stays case-sensitive" do
      match "Payment to AMAZON" (Matcher Contains "amazon") `shouldBe` False
      match "Payment to amazon" (Matcher Contains "amazon") `shouldBe` True
