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
tests = testGroup "Parser"
  [ testGroup "El Parsers"
    [ testCase "Raw Text" $ testParser normalText "This is a 1. piece of normal ` text ~!" (Raw "This is a 1. piece of normal ` text ~!")
    , testCase "Strong Text" $ testParser strongText "__A strong piece of text__" (Strong [ Raw "A strong piece of text"])
    , testCase "Emph Text" $ testParser emphText "_A emph piece of text_" (Emphasis [Raw "A emph piece of text"])
    , testCase "Nested emph" $ testParser emphText "_A emph __and strong__ piece of text_" (Emphasis [Raw "A emph ", Strong [Raw "and strong"], Raw " piece of text"])
    , testCase "Nested strong" $ testParser strongText "___strong and emph___" (Strong [Emphasis [ Raw "strong and emph"]])
    , testCase "Strict Text" $ testParser strictText "This is a 1. piece of normal ` text ~!" (Raw "This is a ")
    , testCase "Without Gaps" $ testParser withoutGapsText "this has to end without a space" (Raw "this has to end without a space")
    , testCase "Without Gaps failure" $ testFail withoutGapsText "this has to end without a space "
    , testCase "Indented Codeblock" $ testParser indentedCodeBlock "\n  \n    codeblock starts\n     continues^123~\n" (Codeblock "codeblock starts\n continues^123~\n")
    , testCase "Fenced Codeblock" $ testParser fencedCodeBlock " ````\n codeblock starts\n  continues^123~\n````\n" (Codeblock "codeblock starts\n continues^123~\n")
    ]
  , testGroup "Top Parsers"
    [ testCase "Title depth 1" $ testParser titleParser " # A title ######### \n" (Title 1 [Raw "A title "])
    , testCase "Title depth 3" $ testParser titleParser " ### A title ######### \n" (Title 3 [Raw "A title "])
    , testCase "Title depth 7 fail" $ testFail titleParser " ####### A title ######### \n"
    ]
  ]
