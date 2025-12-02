module Day1

import Data.String

scanl : (b -> a -> b) -> b -> List a -> List b
scanl f acc [] = [acc]
scanl f acc (x :: xs) = acc :: scanl f (f acc x) xs

toRotate : String -> Maybe Int
toRotate str = case unpack str of
  'L' :: rest => Just (negate (cast (pack rest)))
  'R' :: rest => Just         (cast (pack rest))
  _ => Nothing

countZeros : Int -> Int -> Int
countZeros stt rot =
  let end = (stt + rot) `mod` 100
      overflow  = if rot < 0 && end > stt && stt /= 0
                    then 1
                  else if rot > 0 && stt > end && end /= 0
                    then 1
                  else 0
  in overflow + abs (rot) `div` 100

rotateWithRemainder : (Int, Int) -> Int -> (Int, Int)
rotateWithRemainder (stt, _) rot = 
  let end = (stt + rot) `mod` 100
      zeros = countZeros stt rot
  in (end, zeros)

export
part1 : String -> String
part1 input =
  let rotations = mapMaybe toRotate (lines input)
      posXCross = scanl rotateWithRemainder (50, 0) rotations
      zeros     = filter (==0) (map fst posXCross)
  in cast (length zeros)

export
part2 : String -> String
part2 input =
  let rotations = mapMaybe toRotate (lines input)
      posXCross = scanl rotateWithRemainder (50, 0) rotations
      zeros     = filter (==0) (map fst posXCross)
      cross     = map snd posXCross 
  in cast (cast (length zeros) + sum cross)
