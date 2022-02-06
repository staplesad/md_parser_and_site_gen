{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Directory (listDirectory)

type Parser = Parsec Void String

data El =  Br | Link El String | Image El String | Emphasis El | Strong El | Strikethrough El
        | Blockquote [TopLevel] | UList [[El]]| OList [[El]] | Codespan String | Codeblock String | Raw String
  deriving (Show)

data TopLevel = Title Int String | Block [El] | Hr
  deriving (Show)

type Markdown = [TopLevel]



path :: FilePath
path = "palimpsest/"

filesToParse :: IO [FilePath]
filesToParse = listDirectory path

file :: IO FilePath
file = fmap head filesToParse

data Predicate' a = Predicate' {getPredicate' :: a -> Bool}
instance Semigroup (Predicate' a) where
  f <> g = Predicate' $ \a -> getPredicate' f a || getPredicate' g a
instance Monoid (Predicate' a) where
  mempty = Predicate' $ const True


isHSpace :: Predicate' Char
isHSpace = Predicate' $ \x -> isSpace x && x /= '\n' && x /= '\r'

titleParser :: Parser TopLevel
titleParser = do
  octothorpes <- some (char '#')
  let depth = length octothorpes
  _ <- hspace
  text <- some (alphaNumChar <|> separatorChar)
  _ <- many (char '#' <|> separatorChar)
  _ <- eol
  return (Title depth text)


normalText :: Parser El
normalText = do
  text <- takeWhile1P  (Just "normal text") (getPredicate' (Predicate' isLetter <> isHSpace))
  return (Raw text)


blockParser :: Parser TopLevel
blockParser = do
  elements <-  many (normalText <* skipMany space)
  return (Block elements)

topLevel :: Parser Markdown
topLevel = many ((titleParser <|> blockParser) <* skipMany space)

main :: IO ()
main = do
  fp <- file
  contents <- readFile (path ++ fp)
  print ("Contents: " ++ contents)
  parseTest topLevel contents
--  case parse parseMd (path ++ fp) contents of
--    Left error -> print "err:"
--    Right xs ->  print xs
