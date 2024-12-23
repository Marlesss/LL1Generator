module Main (main) where

main :: IO ()
main = putStrLn "HELLO PYTOC"
{-
import Python.Parser
import Python.Lexer
import Translate
import Prettyprinter
import Prettyprinter.Render.Text
import System.IO

main :: IO ()
main = do
  putStrLn "Pytoc started!"
  pythonCode <- readFile "code.py"
  let parsed = parse $ lexer pythonCode
  let translated = pyToC parsed
  withFile "out.c" WriteMode $ (flip hPutDoc) $ pretty translated

-}