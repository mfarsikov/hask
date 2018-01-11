{-
module Main where

import Lib

main :: IO ()
main = someFunc
-}
import Data.List.Split

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' [a] = [a]
sort' [a,b] = if a < b then [a,b] else [b,a]
sort' list =
  let unsortedParts = split' list
      left     = sort' (fst unsortedParts)
      right    = sort' (snd unsortedParts)
  in merge left right

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge first@(xHead:xTail) second@(yHead:yTail)
  | xHead > yHead     = yHead : merge yTail first
  | otherwise         = xHead : merge xTail second

split' :: [a] -> ([a], [a])
split' list = toTuple (chunksOf (halfSize list) list)

toTuple :: [[a]] -> ([a], [a])
toTuple x
 | len == 2 = (x!!0, x!!1)
 | len == 1 = (x!!0, [])
  where len = length x

halfSize :: [a] -> Int
halfSize l = ceiling (fromIntegral (length l) / 2)