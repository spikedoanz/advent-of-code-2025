module Lib
import Data.String
import Data.List

stripPrefix : List Char -> List Char -> Maybe (List Char)
stripPrefix []      ys      = Just ys
stripPrefix (_::_)  []      = Nothing
stripPrefix (d::ds) (y::ys) = if d == y then stripPrefix ds ys else Nothing

breakOn : List Char -> List Char -> (List Char, Maybe (List Char))
breakOn div xs = case stripPrefix div xs of
  Just rest => ([], Just rest)
  Nothing   => case xs of
    []      => ([], Nothing)
    x :: xs' => let (pre, post) = breakOn div xs' in (x :: pre, post)

splitOnChars : List Char -> List Char -> List (List Char)
splitOnChars div xs = case breakOn div xs of
  (group, Nothing)   => [group]
  (group, Just rest) => group :: splitOnChars div rest

export
splitOn : String -> String -> List String
splitOn div xs = map (pack) (splitOnChars (unpack div) (unpack xs))

