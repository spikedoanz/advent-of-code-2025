module Day5

import Data.String
import Data.List
import Data.SortedSet
import Lib

example = """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

e1 = """
3-5
10-14
16-20
12-18
"""

epair : (Int, Int)
epair = (3,5)

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

-- map over avail predicate for all pairs
-- foldr with or

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

boundToRange : (Int, Int) -> SortedSet Int
boundToRange p = fromList [(fst p) .. (snd p)]


allFreshIds : List (Int, Int) -> Nat
allFreshIds l = length $ toList $ foldr union empty $ map boundToRange l

export
part1 : String -> String
part1 input = cast $ length $ getAvailableIds input

export
part2 : String -> String
part2 input =
  case (splitOn "\n\n" input) of
    [left, right] => cast $ allFreshIds $ getRanges left
    _ => ""

