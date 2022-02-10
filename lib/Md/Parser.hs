{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Md.Parser where

import Control.Monad
import Data.Void
import Data.Char
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Text.Pretty.Simple

type Parser = Parsec Void String

data El =  Br | Link El String | Image El String | Emphasis [El] | Strong [El] | Strikethrough [El]
        | Codespan String | Codeblock String | Raw String
  deriving (Show, Eq)

data TopLevel = Title Int [El] | Block [El] | Hr | Empty | Blockquote [TopLevel] | UList [[El]]| OList [[El]]
  deriving (Show, Eq)

type Markdown = [TopLevel]


data Predicate' a = Predicate' {getPredicate' :: a -> Bool}
instance Semigroup (Predicate' a) where
  f <> g = Predicate' $ \a -> getPredicate' f a || getPredicate' g a
instance Monoid (Predicate' a) where
  mempty = Predicate' $ const True


isHSpace :: Predicate' Char
isHSpace = Predicate' $ \x -> isSpace x && x /= '\n' && x /= '\r'

titleParser :: Parser TopLevel
titleParser = label "Title" $ try $ do
  count' 0 3 separatorChar
  octothorpes <- some (char '#')
  let depth = length octothorpes
  _ <- hspace
  text <- some (subsetBlockEl)
  _ <- many (char '#' <|> separatorChar)
  _ <- eol
  return (Title depth text)

myPuncChar :: Parser Char
myPuncChar = choice [ char '.', char ',', char '<', char '/', char '\'', char '"'
                    , char ':', char ';', char '=', char '(', char ')', char '{', char ']'
                    , char ']', char '&', char '^', char '%', char '$', char '€', char '£'
                    , char '@', char '?', char '\\']

strictText :: Parser El
strictText = label "strictText" $ do
  text <- some (letterChar <|> separatorChar <|> myPuncChar)
  return (Raw text)

normalText :: Parser El
normalText = label "normalText" $ do
  text <- some (alphaNumChar <|> separatorChar <|> punctuationChar <|> symbolChar)
  return (Raw text)

withoutGapsText :: Parser El
withoutGapsText = label "withoutGaps" $ try $ do
  char1 <- alphaNumChar <|> myPuncChar
  rest <- optional $ do
    fmap mconcat $ many $ do
      seps <- optional (many separatorChar)
      t <- some $ alphaNumChar <|> myPuncChar
      pure $ fromMaybe "" seps <> t
  return (Raw (char1 : fromMaybe "" rest))

emphText :: Parser El
emphText = try $ do
  charF <- char '*' <|> char '_'
  text <- some $ withoutGapsText <|> subsetBlockEl
  _ <-  char charF
  return (Emphasis text)

strongText :: Parser El
strongText = label "strong" $ try $ do
  c <- try $ do
    c <- char '*' <|> char '_'
    char c
  text <- some $ withoutGapsText <|> subsetBlockEl
  _ <- count 2 (char c)
  return (Strong text)


hrLine :: Parser TopLevel
hrLine = label "hr" $ try $ do
  count' 0 3 separatorChar
  c <- char '*' <|> char '-' <|> char '_'
  count 2 (char c >> optional separatorChar)
  optional (many (char c <|> separatorChar))
  _ <- eol
  return Hr <?> "Hr"

indentedCodeBlock :: Parser El
indentedCodeBlock = label "indented" $ try $ do
  eol *> many separatorChar *> eol
  text <- fmap unlines $ some $ do
    count 4 separatorChar
    line <- some (letterChar <|> separatorChar <|> punctuationChar <|> symbolChar)
    eol
    pure line
  return (Codeblock text)

exceptionPunctuation :: [Char] -> Char -> Bool
exceptionPunctuation c a = notElem a c && isPunctuation a

exceptionSymbol :: [Char] -> Char -> Bool
exceptionSymbol c a = notElem a c && isSymbol a

fencedCodeBlock :: Parser El
fencedCodeBlock = label "fenced" $ try $ do
  indents <- count' 0 3 separatorChar
  let removeIndent = length indents
  openChars :: [Char] <- count 3 (char '`' <|> char '~')
  let closeChar = head openChars
  extraChars <- many $ char closeChar
  let minClose = length (openChars ++ extraChars)
  _ <- eol
  text <- fmap unlines $ many $ do
    count' 0 removeIndent separatorChar
    line <- some (letterChar <|> separatorChar <|> satisfy (exceptionSymbol [closeChar]) <|> punctuationChar)
    eol
    pure line
  count' 0 3 separatorChar
  count minClose (char closeChar) *> many (char closeChar) *> eol
  return (Codeblock text)

subsetBlockEl :: Parser El
subsetBlockEl = strictText <|> strongText <|> emphText

codeBlockEl :: Parser El
codeBlockEl = indentedCodeBlock <|> fencedCodeBlock

blockEl :: Parser El
blockEl = codeBlockEl <|> subsetBlockEl <|> normalText

blockParser :: Parser TopLevel
blockParser = do
  elements <- some blockEl <* (eof <|> void eol)
  return (Block elements)

emptyParser :: Parser TopLevel
emptyParser = do
  skipMany separatorChar
  eol
  return Empty

topLevel :: Parser Markdown
topLevel = many (titleParser <|> hrLine <|> blockParser <|> emptyParser) <* eof
