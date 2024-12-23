{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TypeApplications #-}

module Python.Parser where

import Prelude hiding (LT, GT, EQ)
import Python.Grammar
import Language.Haskell.TH.ParseGen
import qualified Data.List as L
import Unsafe.Coerce(unsafeCoerce)
import Text.Regex.TDFA
import Debug.Trace

parseError _ = error "Parse error"
data ( Token )
    = TokenEq
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDiv
    | TokenLT
    | TokenLE
    | TokenGT
    | TokenGE
    | TokenEqEq
    | TokenNQ
    | TokenAnd
    | TokenOr
    | TokenNot
    | TokenOB
    | TokenCB
    | TokenEOL
    | TokenIf
    | TokenElse
    | TokenOCurly
    | TokenCCurly
    | TokenComma
    | TokenSignat
    | TokenFor
    | TokenWhile
    | TokenIn
    | TokenRIncl
    | TokenRExcl
    | TokenInt Int
    | TokenBool String
    | TokenType String
    | TokenStr String
    | TokenVar String
    deriving Show
name t = case t of
         {TokenEq -> "'='";
          TokenPlus -> "'+'";
          TokenMinus -> "'-'";
          TokenTimes -> "'*'";
          TokenDiv -> "'/'";
          TokenLT -> "'<'";
          TokenLE -> "'<='";
          TokenGT -> "'>'";
          TokenGE -> "'>='";
          TokenEqEq -> "'=='";
          TokenNQ -> "'!='";
          TokenAnd -> "'and'";
          TokenOr -> "'or'";
          TokenNot -> "'not'";
          TokenOB -> "'('";
          TokenCB -> "')'";
          TokenEOL -> "'\\n'";
          TokenIf -> "if";
          TokenElse -> "else";
          TokenOCurly -> "'{'";
          TokenCCurly -> "'}'";
          TokenComma -> "','";
          TokenSignat -> "'::'";
          TokenFor -> "for";
          TokenWhile -> "while";
          TokenIn -> "in";
          TokenRIncl -> "'...'";
          TokenRExcl -> "'..<'";
          TokenInt _ -> "int";
          TokenBool _ -> "bool";
          TokenType _ -> "type";
          TokenStr _ -> "str";
          TokenVar _ -> "var"}
regexs = [("^\\=", const TokenEq),
          ("^\\+", const TokenPlus),
          ("^\\-", const TokenMinus),
          ("^\\*", const TokenTimes),
          ("^\\/", const TokenDiv),
          ("^<", const TokenLT),
          ("^<=", const TokenLE),
          ("^>", const TokenGT),
          ("^>=", const TokenGE),
          ("^==", const TokenEqEq),
          ("^\\!=", const TokenNQ),
          ("^and", const TokenAnd),
          ("^or", const TokenOr),
          ("^not", const TokenNot),
          ("^\\(", const TokenOB),
          ("^\\)", const TokenCB),
          ("^\\n", const TokenEOL),
          ("^if", const TokenIf),
          ("^else", const TokenElse),
          ("^\\{", const TokenOCurly),
          ("^\\}", const TokenCCurly),
          ("^\\,", const TokenComma),
          ("^::", const TokenSignat),
          ("^for", const TokenFor),
          ("^while", const TokenWhile),
          ("^in", const TokenIn),
          ("^...", const TokenRIncl),
          ("^..<", const TokenRExcl),
          ("^[[:digit:]]+", TokenInt . read),
          ("^(true|false)", TokenBool),
          ("^(Int|String|Boolean)", TokenType),
          ("^\"(.*)\"", TokenStr),
          ("^[[:alpha:]][[:alnum:]]*", TokenVar)]
skip = "^[[:blank:]]+"
getVal (t@(TokenEq)) = unsafeCoerce t
getVal (t@(TokenPlus)) = unsafeCoerce t
getVal (t@(TokenMinus)) = unsafeCoerce t
getVal (t@(TokenTimes)) = unsafeCoerce t
getVal (t@(TokenDiv)) = unsafeCoerce t
getVal (t@(TokenLT)) = unsafeCoerce t
getVal (t@(TokenLE)) = unsafeCoerce t
getVal (t@(TokenGT)) = unsafeCoerce t
getVal (t@(TokenGE)) = unsafeCoerce t
getVal (t@(TokenEqEq)) = unsafeCoerce t
getVal (t@(TokenNQ)) = unsafeCoerce t
getVal (t@(TokenAnd)) = unsafeCoerce t
getVal (t@(TokenOr)) = unsafeCoerce t
getVal (t@(TokenNot)) = unsafeCoerce t
getVal (t@(TokenOB)) = unsafeCoerce t
getVal (t@(TokenCB)) = unsafeCoerce t
getVal (t@(TokenEOL)) = unsafeCoerce t
getVal (t@(TokenIf)) = unsafeCoerce t
getVal (t@(TokenElse)) = unsafeCoerce t
getVal (t@(TokenOCurly)) = unsafeCoerce t
getVal (t@(TokenCCurly)) = unsafeCoerce t
getVal (t@(TokenComma)) = unsafeCoerce t
getVal (t@(TokenSignat)) = unsafeCoerce t
getVal (t@(TokenFor)) = unsafeCoerce t
getVal (t@(TokenWhile)) = unsafeCoerce t
getVal (t@(TokenIn)) = unsafeCoerce t
getVal (t@(TokenRIncl)) = unsafeCoerce t
getVal (t@(TokenRExcl)) = unsafeCoerce t
getVal (TokenInt x) = unsafeCoerce x
getVal (TokenBool x) = unsafeCoerce x
getVal (TokenType x) = unsafeCoerce x
getVal (TokenStr x) = unsafeCoerce x
getVal (TokenVar x) = unsafeCoerce x
class Parse_0 a_1
    where {_parse :: [( Token )] -> (a_1, [( Token )])}
instance Parse_0 Program
    where {_parse tkns_2 | inFirst tkns_2 ["'('",
                                           "'\\n'",
                                           "bool",
                                           "for",
                                           "if",
                                           "int",
                                           "str",
                                           "var",
                                           "while"] || inFollow tkns_2 [Just "'}'",
                                                                        Nothing] = let {(kid_3,
                                                                                         tkns_4) = _parse @Statement tkns_2;
                                                                                        (kid_5,
                                                                                         tkns_6) = _parse @Pf tkns_4}
                                                                                    in ((\arg_1 arg_2 -> Program arg_1 arg_2) kid_3 kid_5,
                                                                                        tkns_6)
                         | otherwise = parseError tkns_2}
instance Parse_0 Pf
    where {_parse tkns_7 | inFirst tkns_7 [] || inFollow tkns_7 [Just "'}'",
                                                                 Nothing] = let
                                                                             in (PfEmpty, tkns_7)
                         | inFirst tkns_7 ["'\\n'"] = let {(kid_8,
                                                            tkns_9) = consume tkns_7 "'\\n'";
                                                           (kid_10,
                                                            tkns_11) = _parse @Statement tkns_9;
                                                           (kid_12, tkns_13) = _parse @Pf tkns_11}
                                                       in ((\arg_1 arg_2 arg_3 -> Pf arg_2 arg_3) kid_8 kid_10 kid_12,
                                                           tkns_13)
                         | otherwise = parseError tkns_7}
instance Parse_0 Statement
    where {_parse tkns_14 | inFirst tkns_14 [] || inFollow tkns_14 [Just "'\\n'",
                                                                    Just "'}'",
                                                                    Nothing] = let
                                                                                in (StatementEmpty,
                                                                                    tkns_14)
                          | inFirst tkns_14 ["var"] = let {(kid_15,
                                                            tkns_16) = consume tkns_14 "var";
                                                           (kid_17,
                                                            tkns_18) = consume tkns_16 "'::'";
                                                           (kid_19,
                                                            tkns_20) = consume tkns_18 "type"}
                                                       in ((\arg_1 arg_2 arg_3 -> Define arg_1 (typeToPrimitiveType arg_3)) kid_15 kid_17 kid_19,
                                                           tkns_20)
                          | inFirst tkns_14 ["var"] = let {(kid_21,
                                                            tkns_22) = consume tkns_14 "var";
                                                           (kid_23,
                                                            tkns_24) = consume tkns_22 "'::'";
                                                           (kid_25,
                                                            tkns_26) = consume tkns_24 "type";
                                                           (kid_27,
                                                            tkns_28) = consume tkns_26 "'='";
                                                           (kid_29, tkns_30) = _parse @Expr tkns_28}
                                                       in ((\arg_1 arg_2 arg_3 arg_4 arg_5 -> DefineSet arg_1 (typeToPrimitiveType arg_3) arg_5) kid_21 kid_23 kid_25 kid_27 kid_29,
                                                           tkns_30)
                          | inFirst tkns_14 ["var"] = let {(kid_31,
                                                            tkns_32) = consume tkns_14 "var";
                                                           (kid_33,
                                                            tkns_34) = consume tkns_32 "'='";
                                                           (kid_35, tkns_36) = _parse @Expr tkns_34}
                                                       in ((\arg_1 arg_2 arg_3 -> Set arg_1 arg_3) kid_31 kid_33 kid_35,
                                                           tkns_36)
                          | inFirst tkns_14 ["if"] = let {(kid_37,
                                                           tkns_38) = consume tkns_14 "if";
                                                          (kid_39, tkns_40) = _parse @Expr tkns_38;
                                                          (kid_41, tkns_42) = consume tkns_40 "'{'";
                                                          (kid_43,
                                                           tkns_44) = _parse @Program tkns_42;
                                                          (kid_45, tkns_46) = consume tkns_44 "'}'"}
                                                      in ((\arg_1 arg_2 arg_3 arg_4 arg_5 -> If arg_2 arg_4) kid_37 kid_39 kid_41 kid_43 kid_45,
                                                          tkns_46)
                          | inFirst tkns_14 ["if"] = let {(kid_47,
                                                           tkns_48) = consume tkns_14 "if";
                                                          (kid_49, tkns_50) = _parse @Expr tkns_48;
                                                          (kid_51, tkns_52) = consume tkns_50 "'{'";
                                                          (kid_53,
                                                           tkns_54) = _parse @Program tkns_52;
                                                          (kid_55, tkns_56) = consume tkns_54 "'}'";
                                                          (kid_57,
                                                           tkns_58) = consume tkns_56 "else";
                                                          (kid_59, tkns_60) = consume tkns_58 "'{'";
                                                          (kid_61,
                                                           tkns_62) = _parse @Program tkns_60;
                                                          (kid_63, tkns_64) = consume tkns_62 "'}'"}
                                                      in ((\arg_1 arg_2 arg_3 arg_4 arg_5 arg_6 arg_7 arg_8 arg_9 -> IfElse arg_2 arg_4 arg_8) kid_47 kid_49 kid_51 kid_53 kid_55 kid_57 kid_59 kid_61 kid_63,
                                                          tkns_64)
                          | inFirst tkns_14 ["for"] = let {(kid_65,
                                                            tkns_66) = consume tkns_14 "for";
                                                           (kid_67,
                                                            tkns_68) = consume tkns_66 "var";
                                                           (kid_69, tkns_70) = consume tkns_68 "in";
                                                           (kid_71,
                                                            tkns_72) = _parse @Range tkns_70;
                                                           (kid_73,
                                                            tkns_74) = consume tkns_72 "'{'";
                                                           (kid_75,
                                                            tkns_76) = _parse @Program tkns_74;
                                                           (kid_77,
                                                            tkns_78) = consume tkns_76 "'}'"}
                                                       in ((\arg_1 arg_2 arg_3 arg_4 arg_5 arg_6 arg_7 -> For arg_2 arg_4 arg_6) kid_65 kid_67 kid_69 kid_71 kid_73 kid_75 kid_77,
                                                           tkns_78)
                          | inFirst tkns_14 ["while"] = let {(kid_79,
                                                              tkns_80) = consume tkns_14 "while";
                                                             (kid_81,
                                                              tkns_82) = _parse @Expr tkns_80;
                                                             (kid_83,
                                                              tkns_84) = consume tkns_82 "'{'";
                                                             (kid_85,
                                                              tkns_86) = _parse @Program tkns_84;
                                                             (kid_87,
                                                              tkns_88) = consume tkns_86 "'}'"}
                                                         in ((\arg_1 arg_2 arg_3 arg_4 arg_5 -> While arg_2 arg_4) kid_79 kid_81 kid_83 kid_85 kid_87,
                                                             tkns_88)
                          | inFirst tkns_14 ["'('",
                                             "bool",
                                             "int",
                                             "str",
                                             "var"] = let (kid_89, tkns_90) = _parse @Expr tkns_14
                                                       in ((\arg_1 -> StExpr arg_1) kid_89, tkns_90)
                          | otherwise = parseError tkns_14}
instance Parse_0 Expr
    where {_parse tkns_91 | inFirst tkns_91 ["'('",
                                             "bool",
                                             "int",
                                             "str",
                                             "var"] = let {(kid_92, tkns_93) = _parse @Ar tkns_91;
                                                           (kid_94, tkns_95) = _parse @Ef tkns_93}
                                                       in ((\arg_1 arg_2 -> Expr arg_1 arg_2) kid_92 kid_94,
                                                           tkns_95)
                          | otherwise = parseError tkns_91}
instance Parse_0 Ef
    where {_parse tkns_96 | inFirst tkns_96 [] || inFollow tkns_96 [Just "')'",
                                                                    Just "','",
                                                                    Just "'...'",
                                                                    Just "'..<'",
                                                                    Just "'\\n'",
                                                                    Just "'{'",
                                                                    Just "'}'",
                                                                    Nothing] = let
                                                                                in (EfEmpty,
                                                                                    tkns_96)
                          | inFirst tkns_96 ["'<'"] = let {(kid_97,
                                                            tkns_98) = consume tkns_96 "'<'";
                                                           (kid_99, tkns_100) = _parse @Ar tkns_98}
                                                       in ((\arg_1 arg_2 -> LT arg_2) kid_97 kid_99,
                                                           tkns_100)
                          | inFirst tkns_96 ["'<='"] = let {(kid_101,
                                                             tkns_102) = consume tkns_96 "'<='";
                                                            (kid_103,
                                                             tkns_104) = _parse @Ar tkns_102}
                                                        in ((\arg_1 arg_2 -> LE arg_2) kid_101 kid_103,
                                                            tkns_104)
                          | inFirst tkns_96 ["'>'"] = let {(kid_105,
                                                            tkns_106) = consume tkns_96 "'>'";
                                                           (kid_107,
                                                            tkns_108) = _parse @Ar tkns_106}
                                                       in ((\arg_1 arg_2 -> GT arg_2) kid_105 kid_107,
                                                           tkns_108)
                          | inFirst tkns_96 ["'>='"] = let {(kid_109,
                                                             tkns_110) = consume tkns_96 "'>='";
                                                            (kid_111,
                                                             tkns_112) = _parse @Ar tkns_110}
                                                        in ((\arg_1 arg_2 -> GE arg_2) kid_109 kid_111,
                                                            tkns_112)
                          | inFirst tkns_96 ["'=='"] = let {(kid_113,
                                                             tkns_114) = consume tkns_96 "'=='";
                                                            (kid_115,
                                                             tkns_116) = _parse @Ar tkns_114}
                                                        in ((\arg_1 arg_2 -> EQ arg_2) kid_113 kid_115,
                                                            tkns_116)
                          | inFirst tkns_96 ["'!='"] = let {(kid_117,
                                                             tkns_118) = consume tkns_96 "'!='";
                                                            (kid_119,
                                                             tkns_120) = _parse @Ar tkns_118}
                                                        in ((\arg_1 arg_2 -> NQ arg_2) kid_117 kid_119,
                                                            tkns_120)
                          | otherwise = parseError tkns_96}
instance Parse_0 Ar
    where {_parse tkns_121 | inFirst tkns_121 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_122,
                                                              tkns_123) = _parse @T tkns_121;
                                                             (kid_124,
                                                              tkns_125) = _parse @Arf tkns_123}
                                                         in ((\arg_1 arg_2 -> Ar arg_1 arg_2) kid_122 kid_124,
                                                             tkns_125)
                           | otherwise = parseError tkns_121}
instance Parse_0 Arf
    where {_parse tkns_126 | inFirst tkns_126 [] || inFollow tkns_126 [Just "'!='",
                                                                       Just "')'",
                                                                       Just "','",
                                                                       Just "'...'",
                                                                       Just "'..<'",
                                                                       Just "'<'",
                                                                       Just "'<='",
                                                                       Just "'=='",
                                                                       Just "'>'",
                                                                       Just "'>='",
                                                                       Just "'\\n'",
                                                                       Just "'{'",
                                                                       Just "'}'",
                                                                       Nothing] = let
                                                                                   in (ArfEmpty,
                                                                                       tkns_126)
                           | inFirst tkns_126 ["'+'"] = let {(kid_127,
                                                              tkns_128) = consume tkns_126 "'+'";
                                                             (kid_129,
                                                              tkns_130) = _parse @T tkns_128;
                                                             (kid_131,
                                                              tkns_132) = _parse @Arf tkns_130}
                                                         in ((\arg_1 arg_2 arg_3 -> Plus arg_2 arg_3) kid_127 kid_129 kid_131,
                                                             tkns_132)
                           | inFirst tkns_126 ["'-'"] = let {(kid_133,
                                                              tkns_134) = consume tkns_126 "'-'";
                                                             (kid_135,
                                                              tkns_136) = _parse @T tkns_134;
                                                             (kid_137,
                                                              tkns_138) = _parse @Arf tkns_136}
                                                         in ((\arg_1 arg_2 arg_3 -> Minus arg_2 arg_3) kid_133 kid_135 kid_137,
                                                             tkns_138)
                           | otherwise = parseError tkns_126}
instance Parse_0 T
    where {_parse tkns_139 | inFirst tkns_139 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_140,
                                                              tkns_141) = _parse @D tkns_139;
                                                             (kid_142,
                                                              tkns_143) = _parse @Tf tkns_141}
                                                         in ((\arg_1 arg_2 -> T arg_1 arg_2) kid_140 kid_142,
                                                             tkns_143)
                           | otherwise = parseError tkns_139}
instance Parse_0 Tf
    where {_parse tkns_144 | inFirst tkns_144 [] || inFollow tkns_144 [Just "'!='",
                                                                       Just "')'",
                                                                       Just "'+'",
                                                                       Just "','",
                                                                       Just "'-'",
                                                                       Just "'...'",
                                                                       Just "'..<'",
                                                                       Just "'<'",
                                                                       Just "'<='",
                                                                       Just "'=='",
                                                                       Just "'>'",
                                                                       Just "'>='",
                                                                       Just "'\\n'",
                                                                       Just "'{'",
                                                                       Just "'}'",
                                                                       Nothing] = let
                                                                                   in (TfEmpty,
                                                                                       tkns_144)
                           | inFirst tkns_144 ["'*'"] = let {(kid_145,
                                                              tkns_146) = consume tkns_144 "'*'";
                                                             (kid_147,
                                                              tkns_148) = _parse @D tkns_146;
                                                             (kid_149,
                                                              tkns_150) = _parse @Tf tkns_148}
                                                         in ((\arg_1 arg_2 arg_3 -> Times arg_2 arg_3) kid_145 kid_147 kid_149,
                                                             tkns_150)
                           | inFirst tkns_144 ["'/'"] = let {(kid_151,
                                                              tkns_152) = consume tkns_144 "'/'";
                                                             (kid_153,
                                                              tkns_154) = _parse @D tkns_152;
                                                             (kid_155,
                                                              tkns_156) = _parse @Tf tkns_154}
                                                         in ((\arg_1 arg_2 arg_3 -> Div arg_2 arg_3) kid_151 kid_153 kid_155,
                                                             tkns_156)
                           | otherwise = parseError tkns_144}
instance Parse_0 D
    where {_parse tkns_157 | inFirst tkns_157 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_158,
                                                              tkns_159) = _parse @C tkns_157;
                                                             (kid_160,
                                                              tkns_161) = _parse @Df tkns_159}
                                                         in ((\arg_1 arg_2 -> D arg_1 arg_2) kid_158 kid_160,
                                                             tkns_161)
                           | otherwise = parseError tkns_157}
instance Parse_0 Df
    where {_parse tkns_162 | inFirst tkns_162 [] || inFollow tkns_162 [Just "'!='",
                                                                       Just "')'",
                                                                       Just "'*'",
                                                                       Just "'+'",
                                                                       Just "','",
                                                                       Just "'-'",
                                                                       Just "'...'",
                                                                       Just "'..<'",
                                                                       Just "'/'",
                                                                       Just "'<'",
                                                                       Just "'<='",
                                                                       Just "'=='",
                                                                       Just "'>'",
                                                                       Just "'>='",
                                                                       Just "'\\n'",
                                                                       Just "'{'",
                                                                       Just "'}'",
                                                                       Nothing] = let
                                                                                   in (DfEmpty,
                                                                                       tkns_162)
                           | inFirst tkns_162 ["'or'"] = let {(kid_163,
                                                               tkns_164) = consume tkns_162 "'or'";
                                                              (kid_165,
                                                               tkns_166) = _parse @C tkns_164;
                                                              (kid_167,
                                                               tkns_168) = _parse @Df tkns_166}
                                                          in ((\arg_1 arg_2 arg_3 -> Or arg_2 arg_3) kid_163 kid_165 kid_167,
                                                              tkns_168)
                           | otherwise = parseError tkns_162}
instance Parse_0 C
    where {_parse tkns_169 | inFirst tkns_169 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_170,
                                                              tkns_171) = _parse @Call tkns_169;
                                                             (kid_172,
                                                              tkns_173) = _parse @Cf tkns_171}
                                                         in ((\arg_1 arg_2 -> C arg_1 arg_2) kid_170 kid_172,
                                                             tkns_173)
                           | otherwise = parseError tkns_169}
instance Parse_0 Cf
    where {_parse tkns_174 | inFirst tkns_174 [] || inFollow tkns_174 [Just "'!='",
                                                                       Just "')'",
                                                                       Just "'*'",
                                                                       Just "'+'",
                                                                       Just "','",
                                                                       Just "'-'",
                                                                       Just "'...'",
                                                                       Just "'..<'",
                                                                       Just "'/'",
                                                                       Just "'<'",
                                                                       Just "'<='",
                                                                       Just "'=='",
                                                                       Just "'>'",
                                                                       Just "'>='",
                                                                       Just "'\\n'",
                                                                       Just "'or'",
                                                                       Just "'{'",
                                                                       Just "'}'",
                                                                       Nothing] = let
                                                                                   in (CfEmpty,
                                                                                       tkns_174)
                           | inFirst tkns_174 ["'and'"] = let {(kid_175,
                                                                tkns_176) = consume tkns_174 "'and'";
                                                               (kid_177,
                                                                tkns_178) = _parse @Call tkns_176;
                                                               (kid_179,
                                                                tkns_180) = _parse @Cf tkns_178}
                                                           in ((\arg_1 arg_2 arg_3 -> And arg_2 arg_3) kid_175 kid_177 kid_179,
                                                               tkns_180)
                           | otherwise = parseError tkns_174}
instance Parse_0 Call
    where {_parse tkns_181 | inFirst tkns_181 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_182,
                                                              tkns_183) = _parse @Y tkns_181;
                                                             (kid_184,
                                                              tkns_185) = _parse @Callf tkns_183}
                                                         in ((\arg_1 arg_2 -> Call arg_1 arg_2) kid_182 kid_184,
                                                             tkns_185)
                           | otherwise = parseError tkns_181}
instance Parse_0 Callf
    where {_parse tkns_186 | inFirst tkns_186 [] || inFollow tkns_186 [Just "'!='",
                                                                       Just "')'",
                                                                       Just "'*'",
                                                                       Just "'+'",
                                                                       Just "','",
                                                                       Just "'-'",
                                                                       Just "'...'",
                                                                       Just "'..<'",
                                                                       Just "'/'",
                                                                       Just "'<'",
                                                                       Just "'<='",
                                                                       Just "'=='",
                                                                       Just "'>'",
                                                                       Just "'>='",
                                                                       Just "'\\n'",
                                                                       Just "'and'",
                                                                       Just "'or'",
                                                                       Just "'{'",
                                                                       Just "'}'",
                                                                       Nothing] = let
                                                                                   in (CallfEmpty,
                                                                                       tkns_186)
                           | inFirst tkns_186 ["'('"] = let {(kid_187,
                                                              tkns_188) = consume tkns_186 "'('";
                                                             (kid_189,
                                                              tkns_190) = _parse @Args tkns_188;
                                                             (kid_191,
                                                              tkns_192) = consume tkns_190 "')'"}
                                                         in ((\arg_1 arg_2 arg_3 -> Callf arg_2) kid_187 kid_189 kid_191,
                                                             tkns_192)
                           | otherwise = parseError tkns_186}
instance Parse_0 Y
    where {_parse tkns_193 | inFirst tkns_193 ["'('"] = let {(kid_194,
                                                              tkns_195) = consume tkns_193 "'('";
                                                             (kid_196,
                                                              tkns_197) = _parse @Expr tkns_195;
                                                             (kid_198,
                                                              tkns_199) = consume tkns_197 "')'"}
                                                         in ((\arg_1 arg_2 arg_3 -> Brack arg_2) kid_194 kid_196 kid_198,
                                                             tkns_199)
                           | inFirst tkns_193 ["int"] = let (kid_200,
                                                             tkns_201) = consume tkns_193 "int"
                                                         in ((\arg_1 -> IntVal arg_1) kid_200,
                                                             tkns_201)
                           | inFirst tkns_193 ["bool"] = let (kid_202,
                                                              tkns_203) = consume tkns_193 "bool"
                                                          in ((\arg_1 -> BoolVal arg_1) kid_202,
                                                              tkns_203)
                           | inFirst tkns_193 ["str"] = let (kid_204,
                                                             tkns_205) = consume tkns_193 "str"
                                                         in ((\arg_1 -> StrVal arg_1) kid_204,
                                                             tkns_205)
                           | inFirst tkns_193 ["var"] = let (kid_206,
                                                             tkns_207) = consume tkns_193 "var"
                                                         in ((\arg_1 -> Var arg_1) kid_206,
                                                             tkns_207)
                           | otherwise = parseError tkns_193}
instance Parse_0 Args
    where {_parse tkns_208 | inFirst tkns_208 [] || inFollow tkns_208 [Just "')'"] = let
                                                                                      in (ArgsEmpty,
                                                                                          tkns_208)
                           | inFirst tkns_208 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_209,
                                                              tkns_210) = _parse @Expr tkns_208;
                                                             (kid_211,
                                                              tkns_212) = _parse @Argsf tkns_210}
                                                         in ((\arg_1 arg_2 -> Args arg_1 arg_2) kid_209 kid_211,
                                                             tkns_212)
                           | otherwise = parseError tkns_208}
instance Parse_0 Argsf
    where {_parse tkns_213 | inFirst tkns_213 [] || inFollow tkns_213 [Just "')'"] = let
                                                                                      in (ArgsfEmpty,
                                                                                          tkns_213)
                           | inFirst tkns_213 ["','"] = let {(kid_214,
                                                              tkns_215) = consume tkns_213 "','";
                                                             (kid_216,
                                                              tkns_217) = _parse @Expr tkns_215;
                                                             (kid_218,
                                                              tkns_219) = _parse @Argsf tkns_217}
                                                         in ((\arg_1 arg_2 arg_3 -> Argsf arg_2 arg_3) kid_214 kid_216 kid_218,
                                                             tkns_219)
                           | otherwise = parseError tkns_213}
instance Parse_0 Range
    where {_parse tkns_220 | inFirst tkns_220 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_221,
                                                              tkns_222) = _parse @Expr tkns_220;
                                                             (kid_223,
                                                              tkns_224) = consume tkns_222 "'...'";
                                                             (kid_225,
                                                              tkns_226) = _parse @Expr tkns_224}
                                                         in ((\arg_1 arg_2 arg_3 -> Inclusive arg_1 arg_3) kid_221 kid_223 kid_225,
                                                             tkns_226)
                           | inFirst tkns_220 ["'('",
                                               "bool",
                                               "int",
                                               "str",
                                               "var"] = let {(kid_227,
                                                              tkns_228) = _parse @Expr tkns_220;
                                                             (kid_229,
                                                              tkns_230) = consume tkns_228 "'..<'";
                                                             (kid_231,
                                                              tkns_232) = _parse @Expr tkns_230}
                                                         in ((\arg_1 arg_2 arg_3 -> Exclusive arg_1 arg_3) kid_227 kid_229 kid_231,
                                                             tkns_232)
                           | otherwise = parseError tkns_220}
lexer :: String -> [Token]
lexer [] = []
lexer str = case L.find ((str =~).fst) regexs of
 (Just (r, func)) -> let (_, matched, rest) = str =~ r :: (String, String, String)
                      in trace ("Input was: " ++ (take 50 str) ++ "\nMatched: " ++ matched ++ "\nby regex: " ++ r) $ func matched : lexer rest
 Nothing          -> if str =~ skip :: Bool
                        then let (_, x, rest) = str =~ skip :: (String, String, String)
                             in trace ("Matched skip: " ++ x) $ lexer rest
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

