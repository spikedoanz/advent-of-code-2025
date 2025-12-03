module Day2

import Data.List
import Data.List1
import Data.String

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
    
splitInHalf : String -> (String, String)
splitInHalf s = 
  let halfLength = cast {to=Nat} ((cast {to=Int} (length s)) `div` 2)
  in case splitAt halfLength (unpack s) of
    (fst,snd) => (pack fst, pack snd)

factor : Int -> List Int
factor n = filter (\x => n `mod` x == 0) (range 1 (n-1))

isInvalid : Int -> Bool
isInvalid n = 
  let (left, right) = splitInHalf (cast n)
  in  if length left == length right 
        then (cast {to=Int} left) == (cast {to=Int} right)
      else False 

allEqual : Eq a => List a -> Bool
allEqual list =
  case list of
    [] => False
    [x] => False
    x :: xs => all (== x) xs
     
splitIntoChunksOfSize : Nat -> List a -> List (List a)
splitIntoChunksOfSize size xs = 
  case xs of
    [] => []
    _  => let (chunk, rest) = splitAt size xs
          in chunk :: splitIntoChunksOfSize size rest

isInvalid2 : Int -> Bool
isInvalid2 n = 
  let factors = factor (cast (length (show n)))
      charLists = map (\f => splitIntoChunksOfSize (cast f) (unpack (show n))) factors
      stringLists = map (map pack) charLists
  in any allEqual stringLists

getInvalidsIn : (Int -> Bool) -> (Int, Int) -> List Int
getInvalidsIn fun pair = filter fun (range (fst pair) (snd pair))

export
part1 : String -> String
part1 input = show (sum (map sum (map (getInvalidsIn isInvalid) (mapMaybe toPair (toPairString input)))))

export
part2 : String -> String
part2 input = show (sum (map sum (map (getInvalidsIn isInvalid2) (mapMaybe toPair (toPairString input)))))
