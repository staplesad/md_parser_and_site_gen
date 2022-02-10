module Main where

import Text.Pretty.Simple
import Text.Megaparsec
import System.Directory (listDirectory)

import Md.Parser

path :: FilePath
path = "palimpsest/"

filesToParse :: IO [FilePath]
filesToParse = listDirectory path

file :: IO FilePath
file = fmap head filesToParse

main :: IO ()
main = do
  fp <- file
  contents <- readFile (path ++ fp)
  pPrint ("Contents: " ++ contents ++ "\n")
  case parse topLevel (path ++ fp) contents of
      Left error -> putStrLn $ errorBundlePretty error
      Right xs ->  pPrint xs
