module Day1

import Data.String


example : String
example = """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""

scanl : (b -> a -> b) -> b -> List a -> List b
scanl f acc [] = [acc]
scanl f acc (x :: xs) = acc :: scanl f (f acc x) xs

toRotate : String -> Maybe Int
toRotate str = case unpack str of
  'L' :: rest => Just (negate (cast (pack rest)))
  'R' :: rest => Just         (cast (pack rest))
  _ => Nothing

rotate : Int -> Int -> Int
rotate a b = mod (a + b) 100

export
part1 : String -> String
part1 input =
  let rotations = mapMaybe toRotate (lines input)
      positions = scanl rotate 50 rotations
      zeros     = filter (==0) positions
  in cast (length zeros)


export
part2 : String -> String
part2 input = ""
