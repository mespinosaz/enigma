module Enigma where

import Data.Char
import Data.Tuple

rotateLeft :: Eq a => [a] -> Int -> [a]
rotateLeft xs 0 = xs
rotateLeft (x:xs) n = rotateLeft (xs ++ [x]) (n-1)

rotateRight :: Eq a => [a] -> Int -> [a]
rotateRight xs 0 = xs
rotateRight xs n = rotateRight ((last xs) : (init xs)) (n-1)

rotate :: Eq a => [a] -> Int -> [a]
rotate xs n     | n < 0 = rotateRight xs (abs n)
                | otherwise = rotateLeft xs n

rotor :: ([Char], [Char]) -> Char -> Char
rotor ([],_) a = a
rotor (x:xs, y:ys) a    | a == x = y
                        | otherwise = rotor (xs, ys) a

transform :: [([Char], [Char])] -> Char -> Char
transform (xs) a = foldr rotor a (reverse xs)

reverseRotors :: [([Char], [Char])] -> [([Char], [Char])]
reverseRotors rs = map swap (reverse rs)

rotateFirstRotor :: [Int] -> [Int]
rotateFirstRotor (offset:rs) = (offset + 1) : rs

buildRotor :: Int -> ([Char],[Char])
buildRotor offset = buildWheel (rotate ['A'..'Z'] offset)

buildRotors :: [Int] -> [([Char],[Char])]
buildRotors rs = map buildRotor rs

buildWheel :: [Char] -> ([Char], [Char])
buildWheel xs = (['A'..'Z'], xs)

buildEnigmaTransformations :: [Char] -> [Int] -> [Char] -> [([Char], [Char])]
buildEnigmaTransformations pb rs ref = (buildWheel pb) : (buildRotors rs)
                                ++ [buildWheel ref]
                                ++ (reverseRotors ((buildWheel pb) : (buildRotors rs)))

enigma :: [Char] -> [Int] -> [Char] -> [Char] -> [Char]
enigma _ _ _ [] = []
enigma pb rs ref (s:ss) =  transform (buildEnigmaTransformations pb rs ref) (toUpper s)
                            : enigma pb (rotateFirstRotor rs) ref ss
