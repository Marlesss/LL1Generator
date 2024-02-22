{-# LANGUAGE TypeApplications #-}

module Main where

import Parser

main :: IO ()
main = do putStrLn "Enter C-function header"
          str <- getLine
          let parsed = fst $ _parse @S $ lexer str
          putStrLn $ show parsed

example = "float* fooBar(int x  ,  char V1ry8adName432);"