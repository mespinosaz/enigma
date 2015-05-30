module Enigma where

import Data.Char

rotateLeft :: Eq a => [a] -> Int -> [a]
rotateLeft xs 0 = xs
rotateLeft (x:xs) n = rotateLeft (xs ++ [x]) (n-1)

rotateRight :: Eq a => [a] -> Int -> [a]
rotateRight xs 0 = xs
rotateRight xs n = rotateRight ([last xs] ++ init xs) (n-1)

rotate :: Eq a => [a] -> Int -> [a]
rotate xs n = if (n < 0) then rotateRight xs (abs n) else rotateLeft xs n

rotor :: ([Char], [Char]) -> Char -> Char
rotor ([],_) _ = '-';
rotor (x:xs, y:ys) a    | a == x = y
                        | a == ' ' = ' '
                        | otherwise = rotor (xs, ys) a

transform :: [([Char], [Char])] -> Char -> Char
transform [] a = a
transform (x:xs) a = transform xs (rotor x a)

reverseRotor :: ([Char], [Char]) -> ([Char], [Char])
reverseRotor (a, b) = (b, a)

reverseRotors :: [([Char], [Char])] -> [([Char], [Char])]
reverseRotors [] = []
reverseRotors (r:rs) = reverseRotors rs ++ [reverseRotor r]

listOfTransformations :: ([Char], [Char]) -> [([Char], [Char])] -> ([Char], [Char]) -> [([Char], [Char])]
listOfTransformations pb rs ref = [pb] ++ rs ++ [ref] ++ (reverseRotors rs) ++ [reverseRotor pb]

rotateFirstRotor :: [Int] -> [Int]
rotateFirstRotor (offset:rs) = (offset + 1) : rs

generateRotor :: Int -> ([Char],[Char])
generateRotor offset = (['A'..'Z'], rotate ['A'..'Z'] offset)

generateRotors :: [Int] -> [([Char],[Char])]
generateRotors [] = []
generateRotors (r:rs) = generateRotor r : generateRotors rs

generatePlugboard :: [Char] -> ([Char], [Char])
generatePlugboard pb = (['A'..'Z'], pb)

generatorReflector :: [Char] -> ([Char], [Char])
generatorReflector ref = (['A'..'Z'], ref)

enigma :: [Char] -> [Int] -> [Char] -> [Char] -> [Char]
enigma _ _ _ [] = []
enigma pb rs ref (s:ss) =  [transform (listOfTransformations (generatePlugboard pb) (generateRotors rs) (generatorReflector ref) ) s]
                            ++ enigma pb (rotateFirstRotor rs) ref ss
