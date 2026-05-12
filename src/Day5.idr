module Day5

import Data.String
import Data.List
import Data.SortedSet
import Lib

getParts : String -> Maybe (String, String)
getParts input =
  case (splitOn "\n\n" input) of
    [left, right] => Just (left, right)
    _ => Nothing

getPair : String -> Maybe (Int, Int)
getPair s =
  case (splitOn "-" s) of
    [left, right] => Just (cast left, cast right)
    _ => Nothing

getRanges : String -> List (Int, Int)
getRanges s = mapMaybe getPair (splitOn "\n" s) 

pairToRange : Maybe (Int, Int) -> SortedSet Int
pairToRange r = 
  case r of
    Just r => fromList [(fst r) .. (snd r)]
    _ => empty

isIn : Int -> (Int, Int) -> Bool
isIn i p = (fst p) <= i && i <= (snd p)

or : Bool -> Bool -> Bool
or a b = a || b

isAvailable : List (Int, Int) -> Int -> Bool
isAvailable l i = foldr or False $ map (isIn i) l

getAvailableIds : String -> List Int
getAvailableIds s =
  case (splitOn "\n\n" s) of
    [left, right] => 
      let bounds = getRanges left
          ids  = map (cast{to=Int}) (lines right)
      in filter (isAvailable bounds) ids
    _ => []

mergeSorted : List (Int, Int) -> List (Int, Int)
mergeSorted [] = []
mergeSorted (x :: xs) = go [x] xs
  where
    go : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
    go p [] = p
    go [] (y :: ys) = go [y] ys 
    go ((lo1, hi1) :: acc) ((lo2, hi2) :: ys) =
      if lo2 <= hi1 + 1
         then go ((lo1, max hi1 hi2) :: acc) ys
         else go ((lo2, hi2) :: (lo1, hi1) :: acc) ys

rangeLen : (Int, Int) -> Int
rangeLen (l, r) = r - l + 1

-- default Ord for (Int, Int) sorts lexographically, so this works out
countValids : List (Int, Int) -> Int
countValids l = sum $ map rangeLen $ mergeSorted $ sort l 

export
part1 : String -> String
part1 input = cast $ length $ getAvailableIds input

export
part2 : String -> String
part2 input =
  case (splitOn "\n\n" input) of
    [left, right] => cast $ countValids $ getRanges left
    _ => ""
