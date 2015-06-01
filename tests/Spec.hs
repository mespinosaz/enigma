import Test.Hspec
import Enigma

main :: IO ()
main = hspec $ do
    describe "enigma function" $ do
        it "returns encoded value" $ do
            enigma "VZBRGITYUPSDNHLXAWMJQOFECK" [1, 88 ,3] "YRUHQSLDPXNGOKMIEBFZCWVJAT" "THIS TEXT WILL BE ENCODED"
            `shouldBe` ("AAPU CXHR TQNJ DT FWJGNUA" :: [Char])
        it "returns decoded value" $ do
            enigma "VZBRGITYUPSDNHLXAWMJQOFECK" [1, 88 ,3] "YRUHQSLDPXNGOKMIEBFZCWVJAT" "AAPU CXHR TQNJ DT FWJGNUA"
            `shouldBe` ("THIS TEXT WILL BE ENCODED" :: [Char])