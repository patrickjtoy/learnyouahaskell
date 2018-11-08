import qualified Data.List as List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (List.tails haystack)
