module Day3

import Data.List1
import Data.String

charToNat : Char -> Maybe Nat
charToNat c =
  if c >= '0' && c <= '9'
    then Just (cast (ord c - ord '0'))
    else Nothing

getNums : String -> List (List Nat)
getNums input = (\x => mapMaybe charToNat x) <$> unpack <$> (lines input)

stackNums : Nat -> List Nat -> Nat
stackNums acc []        = acc
stackNums acc (x :: xs) = stackNums (acc * 10 + x) xs

findMaxIdx : List1 Nat -> (Nat, Integer)
findMaxIdx xs = 
  let indexed = zipWith (\v, i => (v, i)) xs (0 ::: [1 .. cast (length (forget xs))])
  in foldl1 (\a, b => if fst b > fst a then b else a) indexed

pickBest : (remaining : Nat) -> List Nat -> (Nat, List Nat)
pickBest remaining xs = 
  let window = (length xs `minus` remaining) + 1
      pre = take window xs
  in case fromList pre of
       Nothing => (0, [])
       Just pre1 => 
         let (maxVal, idx) = findMaxIdx pre1
             rest = drop (cast idx + 1) xs
         in (maxVal, rest)

getLargestK : Nat -> List Nat -> List Nat
getLargestK Z _ = []
getLargestK _ [] = []
getLargestK (S k) xs = 
  let (best, rest) = pickBest (S k) xs
  in best :: getLargestK k rest

solve n input = show $ sum $ (stackNums 0) <$> (getLargestK n) <$> getNums input

export
part1 : String -> String
part1 input = solve 2 input

export
part2 : String -> String
part2 input = solve 12 input
