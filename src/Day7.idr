module Day7


import Data.String
import Data.SortedSet

parseGrid : String -> List (List Char)
parseGrid input = 
  filter (\n => length n /= 0) $ map unpack (lines input)

getCoord : Char -> List Char -> SortedSet Int 
getCoord _ [] = empty
getCoord c (x :: xs) = fromList $ go (x :: xs) 0
  where
    go : List Char -> Int -> List Int
    go [] _ = []
    go (y :: ys) i =
      if y == c then [i] ++ (go ys (i+1))
      else (go ys (i+1))

splitCoord : List Int -> List Int
splitCoord [] = []
splitCoord (x :: xs) = (x - 1 :: x + 1 :: (splitCoord xs))


propagateRay : SortedSet Int -> List Char -> (Int, SortedSet Int)
propagateRay curr []  = (0, curr)
propagateRay curr next =
  let hats = getCoord '^' next
      pass = difference curr hats 
      hits = intersection curr hats 
      news = fromList $ splitCoord $ SortedSet.toList hits
  in (
    cast $ length $ SortedSet.toList hits,
    union pass news
  )

doPropagate : List (List Char) -> SortedSet Int -> Int
doPropagate [] _ = 0
doPropagate (x :: xs) curr =
  let (splits, next) = propagateRay curr x
  in splits + (doPropagate xs next)

solve : List (List Char) -> Int
solve [] = 0
solve (row :: rows) =
  doPropagate rows (getCoord 'S' row)

export
part1 : String -> String
part1 input = cast $ solve $ parseGrid input

--------------------------------------------------------------------------------

getAt : Int -> List a -> Maybe a
getAt i xs =
  if i < 0 then Nothing
  else go i xs
  where
    go : Int -> List a -> Maybe a
    go _ [] = Nothing
    go 0 (x :: xs) = Just x
    go n (x :: xs) = go (n - 1) xs

QState : Type
QState = List (Int, Int)

getQEntries : (Int, Int) -> List (Int, Int)
getQEntries (idx, count) =
  [(idx - 1, count), (idx + 1, count)]

doMergeQEntries : List (Int, Int) -> List (Int, Int)
doMergeQEntries [] = []
doMergeQEntries (x :: xs) = go x xs
  where 
    go : (Int, Int) -> List (Int, Int) -> List (Int, Int)
    go curr [] = [curr]
    go (currIdx, currCount) ((nextIdx, nextCount) :: rest) =
      if currIdx == nextIdx
        then go (currIdx, currCount + nextCount) rest
        else (currIdx, currCount) :: go (nextIdx, nextCount) rest

mergeQEntries : List (Int, Int) -> List (Int, Int)
mergeQEntries xs =
  doMergeQEntries $ sort xs

propagateOneQRay : List Char -> (Int, Int) -> List (Int, Int)
propagateOneQRay row (idx, count) =
  case getAt idx row of
    Just '^' => [(idx - 1, count), (idx + 1, count)]
    Just '.' => [(idx, count)]
    Just 'S' => [(idx, count)]
    _        => []

propagateQRay : QState -> List Char -> QState
propagateQRay curr row =
  mergeQEntries $ concat $ map (propagateOneQRay row) curr

doQPropagate : List (List Char) -> QState -> QState
doQPropagate [] curr = curr
doQPropagate (row :: rows) curr =
  doQPropagate rows (propagateQRay curr row)

makeQStart : SortedSet Int -> QState
makeQStart starts =
  map (\idx => (idx, 1)) $ Prelude.toList starts

sumQState : QState -> Int
sumQState [] = 0
sumQState ((idx, count) :: rest) =
  count + sumQState rest

solveQ : List (List Char) -> Int
solveQ [] = 0
solveQ (row :: rows) =
  let start = makeQStart $ getCoord 'S' row
      final = doQPropagate rows start
  in sumQState final

export
part2 : String -> String
part2 input =
  cast $ solveQ $ parseGrid input
