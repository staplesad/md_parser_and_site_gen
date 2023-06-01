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

type Document = [Block]

data InlineElem = RawString String

data Mchars = Line String | LineEnding
  deriving (Show)

instance Eq Mchars where
  Line s1 == Line s2 = s1 == s2
  LineEnding == LineEnding = True

data Block = BList [Block] | OList [Block] | Title Int [Mchars] | Break | CodeBlock [Mchars] | Paragraph [Mchars] | BlankLine
  deriving (Show, Eq)

myEOL = void (chunk "\r\n" <|> chunk "\n" <|> chunk "\r") <|> eof

lineEndParser :: Parser Mchars
lineEndParser = LineEnding <$ myEOL
-- fmap (\_ -> LineEnding) (chunk ....)

lineParser :: Parser Mchars
lineParser = do
  hspace
  a <- textFirstChar
  b <- takeWhileP (Just "Bare String") (`notElem` ("\r\n" :: String))
  lineEndParser
  pure $ Line (a:b)

titleParser :: Parser Block
titleParser = do
  --pure (Title 0 [LineEnding])

textFirstChar :: Parser Char
textFirstChar = satisfy (`notElem` ("*-#`0123456789" :: String))

paragraphParser :: Parser Block
paragraphParser = Paragraph <$> some lineParser

--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<$) :: Functor f => b -> f a -> f b

blankParser :: Parser Block
blankParser = BlankLine <$ (hspace >> lineEndParser)
-- hspace uses takeWhileP and consumes tokens so this will not backtrack.
-- blankParser should be the last topLevel block tried for this reason

blockOption :: Parser Block
blockOption = titleParser <|> blankParser

document :: Parser Document
document = many blockOption <* eof
