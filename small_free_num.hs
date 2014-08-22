module SmallFreeNum where
import           Data.List (sort)

(\\) :: [Int] -> [Int] -> [Int]
us \\ vs = filter (\x -> not $ elem x vs) us

minfree :: [Int] -> Int
minfree xs = head ([0 .. ] \\ xs)

test = [08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06]
main = minfree test

