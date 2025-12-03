module Day3

import Data.List1
import Data.String

example = """
987654321111111
811111111111119
234234234234278
818181911112111
"""

charToNat : Char -> Maybe Nat
charToNat c = case c of
  '0' => Just 0 
  '1' => Just 1 
  '2' => Just 2 
  '3' => Just 3 
  '4' => Just 4 
  '5' => Just 5 
  '6' => Just 6 
  '7' => Just 7 
  '8' => Just 8 
  '9' => Just 9 
  _ => Nothing


stackListNums : List Nat -> List Nat
stackListNums xs = [ x*10+y | (i,x) <- zip ([0 .. (length xs)]) xs,
                          (j,y) <- zip ([0 .. (length xs)]) xs,
                          i /= j && i < j] 

popMax : Ord a => Eq a => List1 a -> (a, List a)
popMax (x ::: xs) = 
  let maxElem = foldl max x xs
  in (maxElem, delete maxElem (x :: xs))


stackNums : Nat -> List Nat -> Nat
stackNums acc []        = acc
stackNums acc (x :: xs) = stackNums (acc * 10 + x) xs

getNMax : Nat -> List Nat -> List Nat
getNMax Z _ = []
getNMax _ [] = []
getNMax (S k) (x :: xs) = 
  let (maxElem, remList) = popMax (x ::: xs)
  in maxElem :: getNMax k remList

maximum : Ord a => List1 a -> a
maximum (x ::: xs) = (foldl max x xs)

findMaxIdx : List1 Nat -> (Nat, Nat)
findMaxIdx xs = maximum $ zipWith (\v, i => (v, i)) xs (0 ::: [1 .. length (forget xs)])

pickBest : (remaining : Nat) -> List Nat -> (Nat, List Nat)
pickBest remaining xs = 
  let window = length xs `minus` remaining + 1
      pre = take window xs
  in case fromList pre of
       Nothing => (0, [])
       Just pre1 => 
         let (maxVal, idx) = findMaxIdx pre1
             rest = drop (idx + 1) xs
         in (maxVal, rest)

-- Build the largest k-digit number
getLargestK : Nat -> List Nat -> List Nat
getLargestK Z _ = []
getLargestK _ [] = []
getLargestK (S k) xs = 
  let (best, rest) = pickBest (S k) xs
  in best :: getLargestK k rest


nums_ = map (\x => mapMaybe charToNat x) (map unpack (lines example))
largest2s_ = map (getLargestK 2) nums_


export
part1 : String -> String
part1 input = ""

export
part2 : String -> String
part2 input = ""
