module Main where

import Text.Pretty.Simple
import Text.Megaparsec

import Md.Parser

main :: IO ()
main = do
  fp <- file
  contents <- readFile (path ++ fp)
  pPrint ("Contents: " ++ contents ++ "\n")
  case parse topLevel (path ++ fp) contents of
      Left error -> putStrLn $ errorBundlePretty error
      Right xs ->  pPrint xs
