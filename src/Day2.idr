module Day2

import Data.List
import Data.List1
import Data.String

example = """
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
"""
toPairString : String -> List String
toPairString str = forget (split (==',') str)

toPair : String -> Maybe (Int, Int)
toPair str =
  let strPair = forget (split (=='-') str)
  in case strPair of
    [a, b]  => case (parseInteger a, parseInteger b) of
      (Just x, Just y)  => Just (x,y)
      _                 => Nothing 
    _       => Nothing
    
toPairs : List String -> List (Int, Int)
toPairs xs = mapMaybe toPair xs

{-
the provided pair gives a range [a..b]
the task is to find numbers n ∈ [a..b] s.t it as the structure

x1 x2 x3... xn x1 x2 x3 ... xn

structurally, this means that the number is composed of one number, "repeated twice"

x1...xn * (x1...xn recurse_div 10) + x1...xn

we can check if a given number m is one such invalid number by
1. splitting the string in half
2. checking if the two halves are equal?
-}

splitInHalf : String -> (String, String)
splitInHalf s = 
  let halfLength = cast {to=Nat} ((cast {to=Int} (length s)) `div` 2)
  in case splitAt halfLength (unpack s) of
    (fst,snd) => (pack fst, pack snd)

isInvalid : Int -> Bool
isInvalid n = 
  let (left, right) = splitInHalf (cast n)
  in  if length left == length right 
        then (cast {to=Int} left) == (cast {to=Int} right)
      else False 

range : Int -> Int -> List Int
range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

getInvalidsIn : (Int, Int) -> List Int
getInvalidsIn pair = filter isInvalid (range (fst pair) (snd pair))

export
part1 : String -> String
part1 input = show (sum (map sum (map getInvalidsIn (toPairs (toPairString input)))))

export
part2 : String -> String
part2 input = "part 2 works"
