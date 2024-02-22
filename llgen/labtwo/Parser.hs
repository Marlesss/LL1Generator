{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TypeApplications #-}

module Parser where
import Language.Haskell.TH.ParseGen
import qualified Data.List as L
import Unsafe.Coerce(unsafeCoerce)
import Text.Regex.TDFA


type TWord = String
type TType = String

data S = S R TWord L deriving Show
data R = RT T | RVoid deriving Show
data L = L A Lf | LEmpty deriving Show
data Lf = Lf A Lf | LfEmpty deriving Show
data A = A T TWord deriving Show
data T = T TType Tf deriving Show
data Tf = TfPoint Tf | TfEmpty deriving Show

parseError tkns = error $ "Parse Error!!!: " ++ show tkns
data ( Token )
    = TokenVoid
    | TokenType String
    | TokenWord String
    | TokenLBr
    | TokenRBr
    | TokenSemiC
    | TokenComma
    | TokenAster
    deriving Show
name t = case t of
         {TokenVoid -> "void";
          TokenType _ -> "type";
          TokenWord _ -> "word";
          TokenLBr -> "'('";
          TokenRBr -> "')'";
          TokenSemiC -> "';'";
          TokenComma -> "','";
          TokenAster -> "'*'"}
regexs = [("^void", const TokenVoid),
          ("^\\b(int|float|char|double|long)\\b", TokenType),
          ("^[[:alpha:]][[:alnum:]]*", TokenWord),
          ("^\\(", const TokenLBr),
          ("^\\)", const TokenRBr),
          ("^;", const TokenSemiC),
          ("^,", const TokenComma),
          ("^\\*", const TokenAster)]
skip = "^[[:space:]]+"
getVal (t@(TokenVoid)) = unsafeCoerce t
getVal (TokenType x) = unsafeCoerce x
getVal (TokenWord x) = unsafeCoerce x
getVal (t@(TokenLBr)) = unsafeCoerce t
getVal (t@(TokenRBr)) = unsafeCoerce t
getVal (t@(TokenSemiC)) = unsafeCoerce t
getVal (t@(TokenComma)) = unsafeCoerce t
getVal (t@(TokenAster)) = unsafeCoerce t
class Parse_0 a_1
    where {_parse :: [( Token )] -> (a_1, [( Token )])}
instance Parse_0 S
    where {_parse tkns_2 | inFirst tkns_2 ["type",
                                           "void"] = let {(kid_3, tkns_4) = _parse @R tkns_2;
                                                          (kid_5, tkns_6) = consume tkns_4 "word";
                                                          (kid_7, tkns_8) = consume tkns_6 "'('";
                                                          (kid_9, tkns_10) = _parse @L tkns_8;
                                                          (kid_11, tkns_12) = consume tkns_10 "')'";
                                                          (kid_13, tkns_14) = consume tkns_12 "';'"}
                                                      in ((\arg_1 arg_2 arg_3 arg_4 arg_5 arg_6 -> S arg_1 arg_2 arg_4) kid_3 kid_5 kid_7 kid_9 kid_11 kid_13,
                                                          tkns_14)
                         | otherwise = parseError tkns_2}
instance Parse_0 R
    where {_parse tkns_15 | inFirst tkns_15 ["type"] = let (kid_16,
                                                            tkns_17) = _parse @T tkns_15
                                                        in ((\arg_1 -> RT arg_1) kid_16, tkns_17)
                          | inFirst tkns_15 ["void"] = let (kid_18,
                                                            tkns_19) = consume tkns_15 "void"
                                                        in ((\arg_1 -> RVoid) kid_18, tkns_19)
                          | otherwise = parseError tkns_15}
instance Parse_0 L
    where {_parse tkns_20 | inFirst tkns_20 ["type"] = let {(kid_21,
                                                             tkns_22) = _parse @A tkns_20;
                                                            (kid_23, tkns_24) = _parse @Lf tkns_22}
                                                        in ((\arg_1 arg_2 -> L arg_1 arg_2) kid_21 kid_23,
                                                            tkns_24)
                          | inFirst tkns_20 [] || inFollow tkns_20 [Just "')'"] = let
                                                                                   in (LEmpty,
                                                                                       tkns_20)
                          | otherwise = parseError tkns_20}
instance Parse_0 Lf
    where {_parse tkns_25 | inFirst tkns_25 ["','"] = let {(kid_26,
                                                            tkns_27) = consume tkns_25 "','";
                                                           (kid_28, tkns_29) = _parse @A tkns_27;
                                                           (kid_30, tkns_31) = _parse @Lf tkns_29}
                                                       in ((\arg_1 arg_2 arg_3 -> Lf arg_2 arg_3) kid_26 kid_28 kid_30,
                                                           tkns_31)
                          | inFirst tkns_25 [] || inFollow tkns_25 [Just "')'"] = let
                                                                                   in (LfEmpty,
                                                                                       tkns_25)
                          | otherwise = parseError tkns_25}
instance Parse_0 A
    where {_parse tkns_32 | inFirst tkns_32 ["type"] = let {(kid_33,
                                                             tkns_34) = _parse @T tkns_32;
                                                            (kid_35,
                                                             tkns_36) = consume tkns_34 "word"}
                                                        in ((\arg_1 arg_2 -> A arg_1 arg_2) kid_33 kid_35,
                                                            tkns_36)
                          | otherwise = parseError tkns_32}
instance Parse_0 T
    where {_parse tkns_37 | inFirst tkns_37 ["type"] = let {(kid_38,
                                                             tkns_39) = consume tkns_37 "type";
                                                            (kid_40, tkns_41) = _parse @Tf tkns_39}
                                                        in ((\arg_1 arg_2 -> T arg_1 arg_2) kid_38 kid_40,
                                                            tkns_41)
                          | otherwise = parseError tkns_37}
instance Parse_0 Tf
    where {_parse tkns_42 | inFirst tkns_42 ["'*'"] = let {(kid_43,
                                                            tkns_44) = consume tkns_42 "'*'";
                                                           (kid_45, tkns_46) = _parse @Tf tkns_44}
                                                       in ((\arg_1 arg_2 -> TfPoint arg_2) kid_43 kid_45,
                                                           tkns_46)
                          | inFirst tkns_42 [] || inFollow tkns_42 [Just "word"] = let
                                                                                    in (TfEmpty,
                                                                                        tkns_42)
                          | otherwise = parseError tkns_42}
lexer :: String -> [Token]
lexer [] = []
lexer str = case L.find ((str =~).fst) regexs of
 (Just (r, func)) -> let (_, matched, rest) = str =~ r :: (String, String, String)
                      in func matched : lexer rest
 Nothing          -> if str =~ skip :: Bool
                        then let (_, _, rest) = str =~ skip :: (String, String, String)
                             in lexer rest
                        else error $ "unexpected input: " ++ str

consume a@[] _ = parseError a
consume (t:rest) s
  | name t == s = (getVal t, rest)
  | otherwise = parseError (t:rest)

inFirst :: [Token] -> [String] -> Bool
inFirst [] _ = False
inFirst (t:_) fs = elem (name t) fs

inFollow :: [Token] -> [Maybe String] -> Bool
inFollow [] fs = elem Nothing fs
inFollow (t:_) fs = elem (Just (name t)) fs

