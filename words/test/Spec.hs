import Test.Hspec
import Lib
main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should be able to concatenate every line with a new line" $ do
            (formatGrid ["abc", "def", "ghi" ]) `shouldBe` "abc\ndef\nghi"

