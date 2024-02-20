{
module SimpleHaskell
  ( parseExp
  , getExp
  , argName ) where

import Language.Haskell.TH
import Data.Char
import Data.List
import Data.Maybe
}

%name tokensParseExp Infix

%tokentype { Token }
%error { parseError }

%token
  '(' { TokenLB  }
  ')' { TokenRB  }
  iop { TokenIop $$ }
  con { TokenCon $$ }
  fun { TokenFun $$ }
  num { TokenNum $$ }
  arg { TokenArg $$ }
%%

Infix
  : Infix iop App { UInfixE $1 $2 $3 }
  | App           { $1 }

App
  : App Exp { AppE $1 $2 }
  | Exp     { $1 }

Exp
  : '(' Infix ')' { $2 }
  | '(' iop ')' { $2 }
  | con         { $1 }
  | fun         { $1 }
  | num         { $1 }
  | arg         { $1 }

{
data Token
  = TokenLB
  | TokenRB
  | TokenIop Exp
  | TokenCon Exp
  | TokenFun Exp
  | TokenNum Exp
  | TokenArg Exp
  deriving Show

knownOps = ["++", "==", "||", "&&", "+", "-", "*", "/", ".", "$"]

lexer :: String -> [Token]
lexer [] = []
lexer ('(':rest) = TokenLB : lexer rest
lexer (')':rest) = TokenRB : lexer rest
lexer s@(c:rest)
  | c == '$' && not (null rest) && isNumber (head rest) = lexArg s
  | any (\o -> isPrefixOf o s) knownOps = lexOp s
  | isSpace c = lexer rest
  | isAlpha c = lexName s
  | isNumber c = lexNum s
  | otherwise = error $ "Unknown symbol " ++ s

lexOp :: String -> [Token]
lexOp s = let op = fromJust $ find (\o -> isPrefixOf o s) knownOps in
          TokenIop (VarE (mkName op)) : lexer (fromJust $ stripPrefix op s)

lexName :: String -> [Token]
lexName s@(c:_) = let (n, rest) = span isAlphaNum s
                      name = mkName n in
                  (if (isUpper c) then TokenCon (ConE name) else TokenFun (VarE name)): lexer rest

lexNum s = let (n, rest) = span isNumber s in
           TokenNum (LitE (IntegerL (read n))) : lexer rest

lexArg ('$':s) = let (n, rest) = span isNumber s in
                 TokenArg (VarE (argName (read n))) : lexer rest

argName :: Int -> Name
argName x = mkName $ "arg_" ++ show x

parseError tkns = error $ "Error during parsing haskell code: " ++ show tkns

getExp inp n = LamE pat $ parseExp inp
  where
    pat = map (\i -> VarP (argName i)) [1..n]

parseExp = tokensParseExp . lexer
}

