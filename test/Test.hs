module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec

import Md.Parser

getParseOutput parser input = runParser parser "" input

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Parser"
  [ testCase "Raw Text" $ getParseOutput normalText "This is a 1. piece of normal ` text ~!" @?= (Right $ Raw "This is a 1. piece of normal ` text ~!")
  ]
