import Enigma
import System.Environment

main = do
    args <- getArgs
    print (enigma "VZBRGITYUPSDNHLXAWMJQOFECK" [1, 88 ,3] "YRUHQSLDPXNGOKMIEBFZCWVJAT" (head args))
