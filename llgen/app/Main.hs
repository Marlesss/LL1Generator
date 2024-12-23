{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Grammar
import Lexer
import Generator
import qualified Data.Text.IO as TIO
import Control.Exception (SomeException, catch)

main :: IO ()
main = do
  putStrLn "Input path of file (without extension)"
  fileName <- getLine
  gramFile <- readFile $ fileName ++ ".gram"
  let tokens = lexer gramFile
  let parsed = parse tokens
  catch (gen parsed fileName) handleErr
  where
    gen parsed fileName = do text <- generate parsed
                             TIO.writeFile (fileName ++ ".hs") text
    handleErr :: SomeException -> IO ()
    handleErr ex = putStrLn $ "Caught exception: " ++ show ex