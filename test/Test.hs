module Main where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec

import Md.Parser

testParser :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParser parser input expected = case runParser parser "" input of
  Left err -> fail $ errorBundlePretty err
  Right a -> a @?= expected

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
    , testCase "multiTextLine2" $ testParser paragraphParser "some text\r\n       another text\r\n" (Paragraph [Line "some text", Line "another text"])
    , testCase "simpleBreak" $ testParser breakParser " ----- \n" Break
    , testCase "complicatedBreak" $ testParser breakParser " ~-*---~~~~----* \n" Break
    , testCase "failBreakSpace" $ testFail breakParser " ------- ~~~~~"
    , testCase "failBreakMin" $ testFail breakParser "~~\n"
    ],
    testGroup "Parent Blocks"
    [
      testCase "title1" $ testParser titleParser " # A Title" (Title 1 (Line "A Title"))
    , testCase "title2" $ testParser titleParser "     ##     title words \n" (Title 2 (Line "title words "))
    , testCase "titleSpace" $ testParser titleParser " # #Title\r" (Title 1 (Line "#Title"))
    , testCase "failTitleBlank" $ testFail titleParser "###      \n"
    , testCase "laaaaaargeTitle" $ testParser titleParser "###### 6" (Title 6 (Line "6"))
    , testCase "basicCodeBlock" $ testParser codeBlockParser "``` Here's some code\n````" (CodeBlock  " Here's some code\n")
    , testCase "backtickCodeBlock" $ testParser codeBlockParser "``` a ` here```" (CodeBlock " a ` here")
    , testCase "noEOLCodeBlock" $ testFail codeBlockParser "```sf``` some more"
    , testCase "moreOpeningCodeBlock" $ testParser codeBlockParser "````lol`````" (CodeBlock "lol")
    , testCase "failMoreOpeningCodeBlock" $ testFail codeBlockParser "````lol```"
    ],
    testGroup "RecursiveBlocks"
    [
      testCase "basicList" $ testParser bListParser "- some text\n-line 2\n" (BList [(Paragraph [Line "some text"], 0), (Paragraph[Line "line 2"], 0)])
    , testCase "indentedList" $ testParser bListParser "  - first\n" (BList [(Paragraph [Line "first"], 2)])
    , testCase "indented2List" $ testParser bListParser "  - first\n  - second\n" (BList [(Paragraph [Line "first"], 2), (Paragraph [Line "second"], 2)])
    , testCase "nestedList" $ testParser bListParser "- first\n  - indent\n" (BList [(Paragraph [Line "first"], 0), (Paragraph [Line "indent"], 2)])
    , testCase "codeBlockList" $ testParser bListParser "- ```abcdef   l```\n- other" (BList [(CodeBlock "abcdef   l", 0), (Paragraph [Line "other"], 0)])
    , testCase "titleBlockList" $ testParser bListParser "- # title\n- other" (BList [(Title 1 (Line "title"), 0), (Paragraph [Line "other"], 0)])
    , testCase "listBlockList" $ testParser bListParser "-  - words\n     - words" (BList [(BList [(Paragraph [Line "words"], 2), (Paragraph [Line "words"], 5)], 0)])
    , testCase "basicOList" $ testParser oListParser "5. words\n1. more words" (OList [(Paragraph [Line "words"], 0), (Paragraph [Line "more words"], 0)])
    , testCase "indentedOList" $ testParser oListParser "1. w\n  2. x\n1. w" (OList [(Paragraph [Line "w"], 0), (Paragraph [Line "x"], 2), (Paragraph [Line "w"], 0)])
    , testCase "mixLists" $ testFail oListParser "1. something\n  - x\n2.  s"
    , testCase "mixListsBlocks" $ testParser (some blockOption) "1. something\n  - x\n2.  s" [OList [(Paragraph [Line "something"], 0)], BList [(Paragraph [Line "x"], 2)], OList [(Paragraph [Line "s"], 0)]]
    , testCase "emptyItem" $ testParser bListParser "- x\n-  \n- y" (BList [(Paragraph [Line "x"], 0), (BlankLine, 0), (Paragraph [Line "y"], 0)])
    ]
    , testGroup "Component Parsers"
    [ testCase "textFirstChar" $ testParser textFirstChar "o" 'o'
    , testCase "failTextFirstChar" $ testFail textFirstChar "*"
    , testCase "indentString" $ testParser (indentStringParser 3) "    \n     dkalmfkasldmf \n ##  \n```" "    \n     dkalmfkasldmf \n ##  \n"
    ]
 ]
