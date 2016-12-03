module Enigma (enigma) where

import Data.Char
import Data.Tuple

rotateLeft :: [a] -> Int -> [a]
rotateLeft xs 0 = xs
rotateLeft (x:xs) n = rotateLeft (xs ++ [x]) (n-1)

rotateRight :: [a] -> Int -> [a]
rotateRight xs 0 = xs
rotateRight xs n = rotateRight (last xs : init xs) (n-1)

rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = rotateRight xs (abs n)
  | otherwise = rotateLeft xs n

rotor :: (String, String) -> Char -> Char
rotor ([],_) a = a
rotor (x:xs, y:ys) a
  | a == x = y
  | otherwise = rotor (xs, ys) a

reverseRotation :: [(String, String)] -> [(String, String)]
reverseRotation rs = map swap (reverse rs)

rotation :: Int -> (String, String)
rotation offset = wheel (rotate ['A'..'Z'] offset)

rotations :: [Int] -> [(String, String)]
rotations = map rotation

wheel :: String -> (String, String)
wheel xs = (['A'..'Z'], xs)

machineState :: String -> [Int] -> String -> [(String, String)]
machineState pb rs ref =
  wheel pb : rotations rs
    ++ [wheel ref]
    ++ reverseRotation (wheel pb : rotations rs)

cipher :: [(String, String)] -> Char -> Char
cipher xs a = foldr rotor a (reverse xs)

enigma :: String -> [Int] -> String -> String -> String
enigma _ _ _ [] = []
enigma pb rs ref (s:ss) =
  cipher machine inputString
    : enigma pb nextRotorPosition ref ss
    where inputString = toUpper s
          machine = machineState pb rs ref
          nextRotorPosition = head rs + 1 : tail rs
