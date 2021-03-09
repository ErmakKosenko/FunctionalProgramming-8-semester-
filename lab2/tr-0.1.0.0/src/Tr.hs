module Tr
    ( CharSet
    , tr
    ) where

import Data.Maybe

type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
-- 
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.

replacementMap :: CharSet -> CharSet -> [(Char, Char)]
replacementMap [] _ = []
replacementMap (x : firstOthers) [y] = (x, y) : replacementMap firstOthers [y]
replacementMap (x : firstOthers) (y : secondOthers) = (x, y) : replacementMap firstOthers secondOthers

tr :: CharSet -> Maybe CharSet -> String -> String
tr first (Just second) = map (\x -> Data.Maybe.fromMaybe x (lookup x (replacementMap first second)))
tr first Nothing = filter (`notElem` first)