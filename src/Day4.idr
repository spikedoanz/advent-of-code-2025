module Day4

import Data.String
import Data.SortedSet

parseGrid : String -> SortedSet (Int, Int)
parseGrid input =
  let rows = lines input
      coords = do
        (y, row) <- zip [0..(length rows)] rows
        (x, col) <- zip [0..(length (unpack row))] (unpack row)
        guard (col == '@')
        pure (cast x, cast y)
  in fromList coords

neighbors : (Int, Int) -> List (Int, Int)
neighbors (y, x) = 
  [ (cast y + dy, cast x + dx)
  | dy <- [-1,0,1]
  , dx <- [-1,0,1]
  , (dy, dx) /= (0,0)
  ]

countAccessible: SortedSet (Int, Int) -> Nat
countAccessible rolls = 
  length $ filter accessible (Prelude.toList rolls)
  where
    accessible : (Int, Int) -> Bool
    accessible p =
      length (filter (\n => contains n rolls) (neighbors p)) < 4

removeAccessibles : SortedSet (Int, Int) -> SortedSet (Int, Int)
removeAccessibles rolls =
  let newRolls = filter (not . accessible) (Prelude.toList rolls)
  in fromList newRolls
  where
    accessible : (Int, Int) -> Bool
    accessible p =
      length (filter (\n => contains n rolls) (neighbors p)) < 4

partial
applyUntilFixedPoint : SortedSet (Int, Int) -> SortedSet (Int, Int)
applyUntilFixedPoint rolls = 
  let newRolls = removeAccessibles rolls 
  in
    if (length $ Prelude.toList newRolls) == (length $ Prelude.toList rolls)
    then newRolls
    else applyUntilFixedPoint newRolls


export
part1 : String -> String
part1 input = cast $ countAccessible $ parseGrid input

export partial
part2 : String -> String
part2 input = 
  let finalLength = cast {to=Int} $ length $ Prelude.toList $ applyUntilFixedPoint $ parseGrid input
      startLength = cast {to=Int} $ length $ Prelude.toList $ parseGrid input
  in cast (startLength - finalLength)
