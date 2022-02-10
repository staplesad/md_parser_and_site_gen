module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec

import Md.Parser

testParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParser parser input expected = runParser parser "" input @?= Right expected

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "El Parsers"
  [ testCase "Raw Text" $ testParser normalText "This is a 1. piece of normal ` text ~!" (Raw "This is a 1. piece of normal ` text ~!")
  , testCase "Strong Text" $ testParser strongText "__A strong piece of text__" (Strong [ Raw "A strong piece of text"])
  , testCase "Emph Text" $ testParser emphText "_A emph piece of text_" (Emphasis [Raw "A emph piece of text"])
  , testCase "Nested emph" $ testParser emphText "_A emph __and strong__ piece of text_" (Emphasis [Raw "A emph ", Strong [Raw "and strong"], Raw " piece of text"])
  , testCase "Nested strong" $ testParser strongText "___strong and emph___" (Strong [Emphasis [ Raw "strong and emph"]])
  , testCase "Strict Text" $ testParser strictText "This is a 1. piece of normal ` text ~!" (Raw "This is a 1. piece of normal ` text ~!")
  ]
