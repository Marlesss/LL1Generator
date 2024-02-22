{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Grammar
import Lexer
import Generator
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "Input path of file (without extension)"
  fileName <- getLine
  gramFile <- readFile $ fileName ++ ".gram"
  let tokens = lexer gramFile
  let parsed = parse tokens
  text <- generate parsed
  TIO.writeFile (fileName ++ ".hs") text
