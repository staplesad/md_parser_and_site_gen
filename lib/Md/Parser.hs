{-# LANGUAGE ScopedTypeVariables #-}

module Md.Parser where

import Debug.Trace
import Control.Monad
import Data.Void
import Data.Char
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Text.Pretty.Simple

type Parser = Parsec Void String

type Document = [Block]

data InlineElem = RawString String

data Mchars = Line String | LineEnding
  deriving (Show)

instance Eq Mchars where
  Line s1 == Line s2 = s1 == s2
  LineEnding == LineEnding = True
  LineEnding == Line _ = False
  Line _ == LineEnding = False

data Block
  = BList [(Block, Int)]
  | OList [(Block, Int)]
  | Title Int Mchars
  | Break
  | CodeBlock String
  | Paragraph [Mchars]
  | BlankLine
  deriving (Show, Eq)

myEOL = void (chunk "\r\n" <|> chunk "\n" <|> chunk "\r") <|> eof

lineEndParser :: Parser Mchars
lineEndParser = LineEnding <$ myEOL
-- fmap . const
-- fmap (\_ -> LineEnding) (chunk ....)

singleLineChar :: Char -> Bool
singleLineChar = (`notElem` ("\r\n" :: String))

lineParser :: Parser Mchars
lineParser = do
  a <- try $ do
    hspace
    textFirstChar
  b <- takeWhileP (Just "Bare String") singleLineChar
  lineEndParser
  pure $ Line (a:b)

titleParser :: Parser Block
titleParser = do
  title_depth <- try $ do
    hspace
    some (char '#')
  let n = length title_depth
  hspace
  title_text <- takeWhile1P (Just "Title String") singleLineChar
  lineEndParser
  pure $ Title n (Line title_text)

breakParser :: Parser Block
breakParser = do
  try $ do
    hspace
    count 3 $ oneOf ("-*~")
  some $ oneOf ("-*~")
  hspace
  lineEndParser
  pure Break :: Parser Block


indentStringParser :: Int -> Parser String
indentStringParser n = do
  s <- takeWhile1P (Just "Codeblock String") (/= '`')
  s' <- try ("" <$ count n (char '`')) <|> continue
  pure $  s <> s'
  where
    continue :: Parser String
    continue = do
      char '`'
      s' <- indentStringParser n
      pure $ '`' : s'

codeBlockParser :: Parser Block
codeBlockParser = do
  try $ do
    hspace
    count 3 $ char '`'
  extra_backticks <- many $ char '`'
  let n = length extra_backticks
  s <- indentStringParser (n+3)
  many (char '`')
  hspace
  lineEndParser
  pure $ CodeBlock s

bListItemParser :: Parser (Block, Int)
bListItemParser = do
  indents <- many $ char ' '
  let n = length indents
  oneOf "-*+"
  b <- bBlockOption
  pure (b, n)

bListParser :: Parser Block
bListParser = try $ do
  b <- some bListItemParser
  pure $ BList b

oListItemParser :: Parser (Block, Int)
oListItemParser = do
  indents <- many $ char ' '
  let n = length indents
  satisfy isDigit
  char '.'
  char ' '
  o <- oBlockOption
  pure (o, n)

oListParser :: Parser Block
oListParser = try $ do
  o <- some oListItemParser
  pure $ OList o

textFirstChar :: Parser Char
textFirstChar = satisfy (`notElem` ("+*-#`0123456789" :: String))

paragraphParser :: Parser Block
paragraphParser = Paragraph <$> some lineParser

--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<$) :: Functor f => b -> f a -> f b

blankParser :: Parser Block
blankParser = try $ BlankLine <$ (hspace >> lineEndParser)
-- hspace uses takeWhileP and consumes tokens so this will not backtrack.
-- blankParser should be the last topLevel block tried for this reason

oBlockOption :: Parser Block
oBlockOption
  = oListParser
  <|> titleParser
  <|> codeBlockParser
  <|> breakParser
  <|> paragraphParser
  <|> blankParser

bBlockOption :: Parser Block
bBlockOption
  = bListParser
  <|> titleParser
  <|> codeBlockParser
  <|> breakParser
  <|> paragraphParser
  <|> blankParser

blockOption :: Parser Block
blockOption
  = bListParser
  <|> oListParser
  <|> titleParser
  <|> codeBlockParser
  <|> breakParser
  <|> paragraphParser
  <|> blankParser

document :: Parser Document
document = many blockOption <* eof
