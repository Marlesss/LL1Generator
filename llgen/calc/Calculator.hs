{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TypeApplications #-}

module Calculator where
import Language.Haskell.TH.ParseGen
import qualified Data.List as L
import Unsafe.Coerce(unsafeCoerce)
import Text.Regex.TDFA

type NumT = Double

data E = E T Ef deriving Show
data Ef
  = Plus T Ef
  | Minus T Ef
  | NoEf
  deriving Show
data T = T F Tf deriving Show
data Tf
  = Mult F Tf
  | Div F Tf
  | NoTf
  deriving Show
data F
  = Brace E
  | N NumT
  | Func String [E]
  deriving Show
data Args
  = ArgsEmpty
  | ArgsNonEmpty E ArgsTail
  deriving Show
data ArgsTail
  = ArgsTailEmpty
  | ArgsTail E ArgsTail
  deriving Show

argsToList :: Args -> [E]
argsToList ArgsEmpty = []
argsToList (ArgsNonEmpty e tail) = e : tailToList tail
  where
    tailToList ArgsTailEmpty = []
    tailToList (ArgsTail e' tail') = e' : tailToList tail'

class Calc a where
  calc :: a -> NumT

instance Calc E where
  calc (E t ef) = (applyEf ef) (calc t)
    where
      applyEf ef = case ef of
        (Plus t ef2)  -> (applyEf ef2).(+ (calc t))
        (Minus t ef2) -> (applyEf ef2).(subtract (calc t))
        NoEf          -> id

instance Calc T where
  calc (T f tf) = (applyTf tf) (calc f)
    where
      applyTf tf = case tf of
        (Mult f tf2) -> (applyTf tf2).(* (calc f))
        (Div f tf2)  -> (applyTf tf2).(/ (calc f))
        NoTf         -> id

instance Calc F where
  calc (Brace e) = calc e
  calc (N x) = x
  calc (Func "gcd" args) = fromIntegral $ foldr1 gcd (map (truncate.calc) args)

parseError tkns = error $ "Parse Error!!!: " ++ show tkns
data ( Token )
    = TokenN NumT
    | TokenLBr
    | TokenRBr
    | TokenPlus
    | TokenMinus
    | TokenMult
    | TokenDiv
    | TokenComma
    | TokenFunc String
    deriving Show
name t = case t of
         {TokenN _ -> "n";
          TokenLBr -> "'('";
          TokenRBr -> "')'";
          TokenPlus -> "'+'";
          TokenMinus -> "'-'";
          TokenMult -> "'*'";
          TokenDiv -> "'/'";
          TokenComma -> "','";
          TokenFunc _ -> "func"}
regexs = [("^[[:digit:]]+", TokenN . read),
          ("^\\(", const TokenLBr),
          ("^\\)", const TokenRBr),
          ("^\\+", const TokenPlus),
          ("^-", const TokenMinus),
          ("^\\*", const TokenMult),
          ("^/", const TokenDiv),
          ("^,", const TokenComma),
          ("^[[:lower:]][[:alnum:]]*", TokenFunc)]
skip = "^[[:space:]]+"
getVal (TokenN x) = unsafeCoerce x
getVal (t@(TokenLBr)) = unsafeCoerce t
getVal (t@(TokenRBr)) = unsafeCoerce t
getVal (t@(TokenPlus)) = unsafeCoerce t
getVal (t@(TokenMinus)) = unsafeCoerce t
getVal (t@(TokenMult)) = unsafeCoerce t
getVal (t@(TokenDiv)) = unsafeCoerce t
getVal (t@(TokenComma)) = unsafeCoerce t
getVal (TokenFunc x) = unsafeCoerce x
class Parse_0 a_1
    where {_parse :: [( Token )] -> (a_1, [( Token )])}
instance Parse_0 E
    where {_parse tkns_2 | inFirst tkns_2 ["'('",
                                           "func",
                                           "n"] = let {(kid_3, tkns_4) = _parse @T tkns_2;
                                                       (kid_5, tkns_6) = _parse @Ef tkns_4}
                                                   in ((\arg_1 arg_2 -> E arg_1 arg_2) kid_3 kid_5,
                                                       tkns_6)
                         | otherwise = parseError tkns_2}
instance Parse_0 Ef
    where {_parse tkns_7 | inFirst tkns_7 ["'+'"] = let {(kid_8,
                                                          tkns_9) = consume tkns_7 "'+'";
                                                         (kid_10, tkns_11) = _parse @T tkns_9;
                                                         (kid_12, tkns_13) = _parse @Ef tkns_11}
                                                     in ((\arg_1 arg_2 arg_3 -> Plus arg_2 arg_3) kid_8 kid_10 kid_12,
                                                         tkns_13)
                         | inFirst tkns_7 ["'-'"] = let {(kid_14,
                                                          tkns_15) = consume tkns_7 "'-'";
                                                         (kid_16, tkns_17) = _parse @T tkns_15;
                                                         (kid_18, tkns_19) = _parse @Ef tkns_17}
                                                     in ((\arg_1 arg_2 arg_3 -> Minus arg_2 arg_3) kid_14 kid_16 kid_18,
                                                         tkns_19)
                         | inFirst tkns_7 [] || inFollow tkns_7 [Just "')'",
                                                                 Just "','",
                                                                 Nothing] = let
                                                                             in (NoEf, tkns_7)
                         | otherwise = parseError tkns_7}
instance Parse_0 T
    where {_parse tkns_20 | inFirst tkns_20 ["'('",
                                             "func",
                                             "n"] = let {(kid_21, tkns_22) = _parse @F tkns_20;
                                                         (kid_23, tkns_24) = _parse @Tf tkns_22}
                                                     in ((\arg_1 arg_2 -> T arg_1 arg_2) kid_21 kid_23,
                                                         tkns_24)
                          | otherwise = parseError tkns_20}
instance Parse_0 Tf
    where {_parse tkns_25 | inFirst tkns_25 ["'*'"] = let {(kid_26,
                                                            tkns_27) = consume tkns_25 "'*'";
                                                           (kid_28, tkns_29) = _parse @F tkns_27;
                                                           (kid_30, tkns_31) = _parse @Tf tkns_29}
                                                       in ((\arg_1 arg_2 arg_3 -> Mult arg_2 arg_3) kid_26 kid_28 kid_30,
                                                           tkns_31)
                          | inFirst tkns_25 ["'/'"] = let {(kid_32,
                                                            tkns_33) = consume tkns_25 "'/'";
                                                           (kid_34, tkns_35) = _parse @F tkns_33;
                                                           (kid_36, tkns_37) = _parse @Tf tkns_35}
                                                       in ((\arg_1 arg_2 arg_3 -> Div arg_2 arg_3) kid_32 kid_34 kid_36,
                                                           tkns_37)
                          | inFirst tkns_25 [] || inFollow tkns_25 [Just "')'",
                                                                    Just "'+'",
                                                                    Just "','",
                                                                    Just "'-'",
                                                                    Nothing] = let
                                                                                in (NoTf, tkns_25)
                          | otherwise = parseError tkns_25}
instance Parse_0 F
    where {_parse tkns_38 | inFirst tkns_38 ["'('"] = let {(kid_39,
                                                            tkns_40) = consume tkns_38 "'('";
                                                           (kid_41, tkns_42) = _parse @E tkns_40;
                                                           (kid_43,
                                                            tkns_44) = consume tkns_42 "')'"}
                                                       in ((\arg_1 arg_2 arg_3 -> Brace arg_2) kid_39 kid_41 kid_43,
                                                           tkns_44)
                          | inFirst tkns_38 ["n"] = let (kid_45,
                                                         tkns_46) = consume tkns_38 "n"
                                                     in ((\arg_1 -> N arg_1) kid_45, tkns_46)
                          | inFirst tkns_38 ["func"] = let {(kid_47,
                                                             tkns_48) = consume tkns_38 "func";
                                                            (kid_49,
                                                             tkns_50) = consume tkns_48 "'('";
                                                            (kid_51,
                                                             tkns_52) = _parse @Args tkns_50;
                                                            (kid_53,
                                                             tkns_54) = consume tkns_52 "')'"}
                                                        in ((\arg_1 arg_2 arg_3 arg_4 -> Func arg_1 (argsToList arg_3)) kid_47 kid_49 kid_51 kid_53,
                                                            tkns_54)
                          | otherwise = parseError tkns_38}
instance Parse_0 Args
    where {_parse tkns_55 | inFirst tkns_55 [] || inFollow tkns_55 [Just "')'"] = let
                                                                                   in (ArgsEmpty,
                                                                                       tkns_55)
                          | inFirst tkns_55 ["'('", "func", "n"] = let {(kid_56,
                                                                         tkns_57) = _parse @E tkns_55;
                                                                        (kid_58,
                                                                         tkns_59) = _parse @ArgsTail tkns_57}
                                                                    in ((\arg_1 arg_2 -> ArgsNonEmpty arg_1 arg_2) kid_56 kid_58,
                                                                        tkns_59)
                          | otherwise = parseError tkns_55}
instance Parse_0 ArgsTail
    where {_parse tkns_60 | inFirst tkns_60 [] || inFollow tkns_60 [Just "')'"] = let
                                                                                   in (ArgsTailEmpty,
                                                                                       tkns_60)
                          | inFirst tkns_60 ["','"] = let {(kid_61,
                                                            tkns_62) = consume tkns_60 "','";
                                                           (kid_63, tkns_64) = _parse @E tkns_62;
                                                           (kid_65,
                                                            tkns_66) = _parse @ArgsTail tkns_64}
                                                       in ((\arg_1 arg_2 arg_3 -> ArgsTail arg_2 arg_3) kid_61 kid_63 kid_65,
                                                           tkns_66)
                          | otherwise = parseError tkns_60}
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

