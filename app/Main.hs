import Data.List.Split

mergeSort :: (Ord a) => [a] -> [a]
mergeSort x = case x of []     -> []
                    a:[]   -> [a]
                    a:b:[] -> if a < b then [a,b] else [b,a]
                    list   -> merge sortedLeft sortedRight
                              where unsortedParts = split' list
                                    sortedLeft    = (mergeSort . fst) unsortedParts
                                    sortedRight   = (mergeSort . snd) unsortedParts

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge left@(lHead:lTail) right@(rHead:rTail)
  | lHead > rHead     = rHead : merge rTail left
  | otherwise         = lHead : merge lTail right

split' :: [a] -> ([a], [a])
split' list = toTuple (chunksOf (halfSize list) list)
              where halfSize list = (ceiling . (/ 2) . fromIntegral . length) list
                    toTuple [x,y] = (x,y)
