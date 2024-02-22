{-# LANGUAGE TypeApplications #-}

module Main where

import Calculator

main :: IO ()
main = do putStrLn "Enter expression"
          line <- getLine
          let parsed = fst $ _parse @E $ lexer line
          putStrLn $ show parsed
          putStrLn $ show $ calc parsed
