module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Tr

type CharSet' = NonEmptyList Char

tr' :: CharSet -> CharSet -> String -> String
tr' inset outset = tr inset (Just outset)

tr'' :: CharSet -> String -> String
tr'' inset = tr inset Nothing

-- | Test harness.
main :: IO ()
main = hspec $ describe "Testing tr" $ do

-- | Test replaceMode.
    describe "Test replaceMode #1" $
      it "a -> b" $
        tr' "a" "b" "a" `shouldBe` "b"

    describe "Test replaceMode #2" $
      it "a -> b" $
        tr' "a" "b" "aaaa" `shouldBe` "bbbb"

    describe "Test replaceMode #3" $
      it "abc -> def" $
        tr' "abc" "def" "abcdef" `shouldBe` "defdef"
  
    describe "Test replaceMode #4" $
      it "abc -> d" $
        tr' "abc" "d" "abcdef" `shouldBe` "ddddef"

    describe "Test replaceMode #5" $
      it "abc -> d" $
        tr' "abc" "de" "abcdef" `shouldBe` "deedef"

    describe "Test replaceMode #6" $
      it "abc -> d" $
        tr' "abc" "defg" "abcdef" `shouldBe` "defdef"

    describe "Test replaceMode #7" $
      it "eo -> oe" $
        tr' "eo" "oe" "hello" `shouldBe` "holle"

-- | Test deleteMode.
    describe "Test deleteMode #1" $
      it "-d a" $
        tr'' "a" "abc" `shouldBe` "bc"
  
    describe "Test deleteMode #2" $
      it "-d a" $
        tr'' "a" "aaa" `shouldBe` ""

    describe "Test deleteMode #3" $
      it "-d abc" $
        tr'' "abc" "abcd" `shouldBe` "d"

    describe "Test deleteMode #4" $
      it "-d abc" $
        tr'' "abc" "dabc" `shouldBe` "d"

    describe "Test deleteMode #5" $
      it "-d abc" $
        tr'' "abc" "dabcdabcabcd" `shouldBe` "ddd"

    describe "Test deleteMode #6" $
      it "-d abcd" $
        tr'' "abcd" "abcd" `shouldBe` ""

    describe "Test deleteMode #7" $
      it "-d abcd" $
        tr'' "abcd" "abce" `shouldBe` "e"

    describe "Test replaceMode #6" $
      it "-d e" $
        tr'' "e" "hello" `shouldBe` "hllo"

-- | QuickCheck tests
    describe "tr quick-check #1" $
      it "empty input is identity" $ property prop_empty_id

    describe "tr quick-check #2" $
      it "length of input doesn't change" $ property prop_non_empty_length

    describe "tr quick-check #3" $
      it "empty input is identity" $ property prop_empty_id_delete

    describe "tr quick-check #4" $
      it "length of input should reduce" $ property prop_non_empty_delete

prop_empty_id :: CharSet' -> CharSet' -> Property
prop_empty_id (NonEmpty set1) (NonEmpty set2)
  = tr' set1 set2 "" === ""

prop_non_empty_length :: CharSet' -> CharSet' -> CharSet' -> Property
prop_non_empty_length (NonEmpty set1) (NonEmpty set2) (NonEmpty str)
  = length (tr' set1 set2 str) === length str

prop_empty_id_delete :: CharSet' -> Property
prop_empty_id_delete (NonEmpty set1)
  = tr'' set1 "" === ""

prop_non_empty_delete :: CharSet' -> CharSet' -> Bool
prop_non_empty_delete (NonEmpty set1) (NonEmpty str)
  = length (tr'' set1 str) <= length str