import Test.Hspec
import Enigma

main :: IO ()
main = hspec $ do
    describe "enigma function" $ do
        it "returns encoded value" $ do
            enigma "VZBRGITYUPSDNHLXAWMJQOFECK" [1, 88 ,3] "YRUHQSLDPXNGOKMIEBFZCWVJAT" "THIS TEXT WILL BE ENCODED AND DECODED"
            `shouldBe` ("AAPU CXHR TQNJ DT FWJGNUA TEM WQYUVZZ" :: [Char])
        it "returns decoded value" $ do
            enigma "VZBRGITYUPSDNHLXAWMJQOFECK" [1, 88 ,3] "YRUHQSLDPXNGOKMIEBFZCWVJAT" "AAPU CXHR TQNJ DT FWJGNUA TEM WQYUVZZ"
            `shouldBe` ("THIS TEXT WILL BE ENCODED AND DECODED" :: [Char])