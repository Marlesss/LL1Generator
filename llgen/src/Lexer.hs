module Lexer
  ( GrammarToken(..)
  , lexer
  , Block
  , NonTermName
  , TermName
  ) where

import Data.Char

data GrammarToken
  = GTPercent
  | GTDelim
  | GTColon
  | GTPipe
  | GTNonTerm String
  | GTTerm String
  | GTBlock String
  | GTRegex String
  deriving Show

type Block = String
type NonTermName = String
type TermName = String

lexer :: String -> [GrammarToken]
lexer [] = []
lexer ('%':'%':cs) = GTDelim   : lexer cs
lexer ('%':cs)     = GTPercent : lexer cs
lexer (':':cs)     = GTColon   : lexer cs
lexer ('|':cs)     = GTPipe    : lexer cs
lexer ('/':cs)     = lexDelim '/' GTRegex cs
lexer ('{':cs)     = lexDelim '}' GTBlock cs
lexer ('\'':cs)    = lexDelim '\'' (\x -> GTTerm $ "'" ++ x ++ "'") cs
lexer ('\"':cs)    = lexDelim '\'' (\x -> GTTerm $ "\"" ++ x ++ "\"") cs
lexer s@(c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexName s
  | otherwise = error $ "Unexpected char " ++ [c]

lexName :: String -> [GrammarToken]
lexName s@(c:_) = let (bs, rest) = span isAlphaNum s in
  (if isUpper c then GTNonTerm else GTTerm) bs : lexer rest
lexName s = error $ "Unknown input: " ++ s

lexDelim :: Char -> (String -> GrammarToken) -> String -> [GrammarToken]
lexDelim c f s = let (bs, rest) = spanAssert c s in f bs : lexer rest

spanAssert :: Char -> String -> (String, String)
spanAssert c s = let (bs, rest) = span (/=c) s in
  if not (null rest) && head rest == c
    then (bs, tail rest)
    else error $ "Expected " ++ [c] ++ " char, got: " ++ rest
