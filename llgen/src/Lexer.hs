module Lexer
  ( GrammarToken(..)
  , lexer
  , Block
  , NonTermName
  , TermName
  ) where

import Data.Char
import Text.Regex.TDFA ((=~))
import Data.List (isPrefixOf)

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
lexer ('{':'=':cs) = lexUntil "=}" GTBlock cs
lexer ('\'':cs)    = lexUntil "'" (\x -> GTTerm $ "'" ++ x ++ "'") cs
lexer ('"':cs) = lexUntil "\"" (\x -> GTTerm $ "\"" ++ x ++ "\"") cs
lexer s@(c:cs)
  | s =~ anyGrouped "/" "/"   = lexDelim (anyGrouped "/" "/") GTRegex s
  | isSpace c        = lexer cs
  | isAlpha c        = lexName s
  | otherwise        = error $ "Unexpected char " ++ '!':c:"!"
                                                  ++ " in context: "
                                                  ++ take 50 s

lexName :: String -> [GrammarToken]
lexName s@(c:_) = let (bs, rest) = span isAlphaNum s in
  (if isUpper c then GTNonTerm else GTTerm) bs : lexer rest
lexName s = error $ "Unknown input: " ++ s

lexDelim :: String -> (String -> GrammarToken) -> String -> [GrammarToken]
lexDelim regex f s = let (_, match, rest) = (s =~ regex :: (String, String, String))
                     in (f.init.tail) match : lexer rest

anyGrouped :: String -> String -> String
anyGrouped l r = "^" ++ l ++ ".*" ++ r

lexUntil :: String -> (String -> GrammarToken) -> String -> [GrammarToken]
lexUntil end f s = let (bs, rest) = spanS end s
  in f bs : lexer rest
  where
    spanS _ ""     = ("", "")
    spanS targ str = if targ `isPrefixOf` str
                     then ("", drop (length targ) str)
                     else let (a, b) = spanS targ (tail str)
                          in ((head str):a, b)
