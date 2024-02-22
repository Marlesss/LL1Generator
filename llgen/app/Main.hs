{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Grammar
import Lexer
import Generator
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "Input name of file (without extension)"
  fileName <- getLine
  gramFile <- readFile $ fileName ++ ".y"
  let tokens = lexer gramFile
  let parsed = parse tokens
  text <- generate parsed
  TIO.writeFile ("./src/" ++ fileName ++ ".hs") text
