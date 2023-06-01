module Main where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec

import Md.Parser

testParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParser parser input expected = runParser parser "" input @?= Right expected

testFail :: Parser a -> String -> Assertion
testFail parser input = isLeft (runParser parser "" input) @?= True

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parse Blocks"
  [ testGroup "Leaf Blocks"
    [ testCase "Blank Line" $ testParser blankParser "  \n" BlankLine
    , testCase "TextLine" $ testParser paragraphParser "some text\n" (Paragraph [Line "some text"])
    , testCase "failTextLine" $ testFail paragraphParser " * sometxt \n"
    , testCase "multiTextLine" $ testParser paragraphParser "some text\n       another text\n" (Paragraph [Line "some text", Line "another text"])
    -- ]
    -- , testGroup "Component Parsers"
    -- [ testCase "textFirstChar" $ testParser textFirstChar "o" 'o'
    -- , testCase "failTextFirstChar" $ testFail textFirstChar "*"
    ]
 ]
