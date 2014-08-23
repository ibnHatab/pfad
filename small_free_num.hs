module SmallFreeNum where

import Data.Array
import Data.List

minfree :: [Int] -> Int
-- minfree xs = head ([0 .. ] \\ xs)

minfree = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n)
          (zip (filter (<= n) xs) (repeat True))
          where n = length xs
 
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

test = [08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06]
main = minfree test

-- blah       :: Int -> Int
-- blah myInt =
--     where 

