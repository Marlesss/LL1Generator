{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Grammar
import Lexer
import Generator
import Data.Text
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  gramFile <- readFile "Gr.y"
  let tokens = lexer gramFile
  let parsed = parse tokens
  text <- generate parsed
  TIO.writeFile "./src/Generated.hs" text
