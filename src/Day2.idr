module Day2

import Data.List
import Data.List1
import Data.String

example = """
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
"""

range : Int -> Int -> List Int
range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

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

splitInHalf : String -> (String, String)
splitInHalf s = 
  let halfLength = cast {to=Nat} ((cast {to=Int} (length s)) `div` 2)
  in case splitAt halfLength (unpack s) of
    (fst,snd) => (pack fst, pack snd)

factor : Int -> List Int
factor n = filter (\x => n `mod` x == 0) (range 1 n)

splitInto : Int -> List a -> List (List a)
splitInto n xs = 
  let chunkSize = cast (length xs) `div` n
  in go xs chunkSize
  where
    go : List a -> Int -> List (List a)
    go [] _ = []
    go xs size = let (chunk, rest) = splitAt (cast size) xs
                 in chunk :: go rest size

isInvalid : Int -> Bool
isInvalid n = 
  let (left, right) = splitInHalf (cast n)
  in  if length left == length right 
        then (cast {to=Int} left) == (cast {to=Int} right)
      else False 

getInvalidsIn : (Int -> Bool) -> (Int, Int) -> List Int
getInvalidsIn fun pair = filter fun (range (fst pair) (snd pair))

allEqual : Eq a => List a -> Bool
allEqual list =
  case list of
    [] => False
    [x] => False
    x :: xs => all (== x) xs
     
strictParseInteger : String -> Maybe Int
strictParseInteger str =
  case parseInteger str of
    Just n => if length (show n) == length str then Just n else Nothing
    _ => Nothing
      
splitIntoChunksOfSize : Nat -> List a -> List (List a)
splitIntoChunksOfSize size xs = 
  case xs of
    [] => []
    _  => let (chunk, rest) = splitAt size xs
          in chunk :: splitIntoChunksOfSize size rest

isInvalid2 : Int -> Bool
isInvalid2 n = 
  let str = show n
      len = length str
      factors = factor (cast len)
      charLists = map (\f => splitIntoChunksOfSize (cast f) (unpack str)) factors
      stringLists = map (map pack) charLists
      intLists = map (mapMaybe strictParseInteger) stringLists
  in any allEqual intLists

export
part1 : String -> String
part1 input = show (sum (map sum (map (getInvalidsIn isInvalid) (toPairs (toPairString input)))))

export
part2 : String -> String
part2 input = show (sum (map sum (map (getInvalidsIn isInvalid2) (toPairs (toPairString input)))))


curr = (map (getInvalidsIn isInvalid2) (toPairs (toPairString example)))
chunks99 = (splitIntoChunksOfSize 1 [9,9])
