module Day6

import Data.String
import Data.List
import Lib

parseGrid : String -> List (List String)
parseGrid s = transpose $ map words $ lines s

castList : List String -> List Int
castList l = map (cast{to=Int}) l

processCol : List String -> Int
processCol l = 
  case (reverse l) of
    "*" :: nums => foldr (*) 1 $ castList nums
    "+" :: nums => foldr (+) 0 $ castList nums
    _ => 0

export
part1 : String -> String
part1 input = cast $ sum $ map processCol $ parseGrid input

--------------------------------------------------------------------------------
-- really not proud of this one. felt a lot like i was doing brute force
-- theorem proving

parseGrid2 : String -> List (List Char)
parseGrid2 s = transpose $ map (reverse . unpack) $ lines s

parseRow : List Char -> Maybe String
parseRow l =
  case filter (\c => c /= ' ') l of 
    [] => Nothing
    _ => Just (pack l)

-- whole empty lines between * and + blocks get split into different lists
splitOnNothing : List (Maybe a) -> List (List a)
splitOnNothing [] = [[]]
splitOnNothing (x :: xs) = case x of
  Nothing => [] :: splitOnNothing xs
  Just x => case splitOnNothing xs of
    chunk :: chunks => (x :: chunk) :: chunks
    [] => [[x]]

-- normalize all numbers to actual numbers
sanitize : List String -> List Int
sanitize l =
  map cast $
  filter (/= "") $
  map (pack . filter isNum . unpack) l

isIn : Char -> List Char -> Bool
isIn c l =
  case l of
    x :: xs => if x == c then True else isIn c xs
    [] => False

containsChar : Char -> List String -> Bool
containsChar c [] = False
containsChar c (x :: xs) = isIn c (unpack x) || containsChar c xs

processRow : List String -> Int
processRow row =
  if containsChar '+' row then
    foldr (+) 0 $ sanitize row
  else if containsChar '*' row then
    foldr (*) 1 $ sanitize row
  else
    0

export
part2 : String -> String
part2 input =
  let rows = splitOnNothing $ map parseRow $ parseGrid2 input
  in cast $ sum $ map processRow rows
