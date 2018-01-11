import Data.List.Split

--sort' :: (Ord a) => [a] -> [a]
sort' x = case x of []     -> []
                    a:[]   -> [a]
                    a:b:[] -> if a < b then [a,b] else [b,a]
                    list   -> merge sortedLeft sortedRight
                              where unsortedParts = split' list
                                    sortedLeft    = (sort' . fst) unsortedParts
                                    sortedRight   = (sort' . snd) unsortedParts

--merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge left@(lHead:lTail) right@(rHead:rTail)
  | lHead > rHead     = rHead : merge rTail left
  | otherwise         = lHead : merge lTail right

split' :: [a] -> ([a], [a])
split' list = toTuple (chunksOf (halfSize list) list)
              where halfSize list = (ceiling . (/ 2) . fromIntegral . length) list

--toTuple :: [[a]] -> ([a], [a])
toTuple [x] = (x,[])
toTuple [x,y] = (x,y)