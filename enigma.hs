module Enigma where

import Data.Char

rotateLeft :: Eq a => [a] -> Int -> [a]
rotateLeft xs 0 = xs
rotateLeft (x:xs) n = rotateLeft (xs ++ [x]) (n-1)

rotateRight :: Eq a => [a] -> Int -> [a]
rotateRight xs 0 = xs
rotateRight xs n = rotateRight ([last xs] ++ init xs) (n-1)

rotate :: Eq a => [a] -> Int -> [a]
rotate xs n     | n < 0 = rotateRight xs (abs n)
                | otherwise = rotateLeft xs n

rotor :: ([Char], [Char]) -> Char -> Char
rotor ([],_) a = a
rotor (x:xs, y:ys) a    | a == x = y
                        | otherwise = rotor (xs, ys) a

transform :: [([Char], [Char])] -> Char -> Char
transform [] a = a
transform (x:xs) a = transform xs (rotor x a)

reverseRotor :: ([Char], [Char]) -> ([Char], [Char])
reverseRotor (a, b) = (b, a)

reverseRotors :: [([Char], [Char])] -> [([Char], [Char])]
reverseRotors [] = []
reverseRotors (r:rs) = reverseRotors rs ++ [reverseRotor r]

rotateFirstRotor :: [Int] -> [Int]
rotateFirstRotor (offset:rs) = (offset + 1) : rs

buildRotor :: Int -> ([Char],[Char])
buildRotor offset = (['A'..'Z'], rotate ['A'..'Z'] offset)

buildRotors :: [Int] -> [([Char],[Char])]
buildRotors [] = []
buildRotors (r:rs) = buildRotor r : buildRotors rs

buildPlugBoard :: [Char] -> ([Char], [Char])
buildPlugBoard pb = (['A'..'Z'], pb)

buildReflector :: [Char] -> ([Char], [Char])
buildReflector ref = (['A'..'Z'], ref)

buildEnigmaTransformations :: [Char] -> [Int] -> [Char] -> [([Char], [Char])]
buildEnigmaTransformations pb rs ref = [buildPlugBoard pb]
                                ++ (buildRotors rs)
                                ++ [buildReflector ref]
                                ++ (reverseRotors (buildRotors rs))
                                ++ [reverseRotor (buildPlugBoard pb)]

enigma :: [Char] -> [Int] -> [Char] -> [Char] -> [Char]
enigma _ _ _ [] = []
enigma pb rs ref (s:ss) =  [transform (buildEnigmaTransformations pb rs ref) (toUpper s)]
                            ++ enigma pb (rotateFirstRotor rs) ref ss
