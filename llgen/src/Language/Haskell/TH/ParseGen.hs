{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.ParseGen where

import Grammar(Production(..), Rule(..))
import Lexer(NonTermName, TermName)
import SimpleHaskell
import Lib
import Language.Haskell.TH.Syntax hiding (Info)
import Language.Haskell.TH.Lib
import Data.Maybe
import Unsafe.Coerce
import qualified Data.Map as DM
import qualified Data.Set as DS

data WrapTerm
  = Term String
  | Eps
  | EOI
  deriving (Show, Ord, Eq)

whileChanges :: Eq a => (a -> a) -> a -> a
whileChanges f a = let b = f a in if a == b then a else whileChanges f b

type ProdMap = DM.Map NonTermName [Rule]
type FFMap = DM.Map NonTermName (DS.Set WrapTerm)
type Sentence = [Either TermName NonTermName]

defLookup :: Ord k => DM.Map k a -> k -> a -> a
defLookup m k def =  fromMaybe def (DM.lookup k m)

firstSent :: FFMap -> Sentence -> DS.Set WrapTerm
firstSent _ [] = DS.singleton Eps
firstSent _ (Left x:_) = DS.singleton (Term x)
firstSent fstMap (Right y:rest) = let firstY = defLookup fstMap y DS.empty in
  DS.union (DS.delete Eps firstY) (if DS.member Eps firstY
                                     then firstSent fstMap rest
                                     else DS.empty)

getFirst :: ProdMap -> FFMap
getFirst prods = whileChanges updFirst empty where
  empty = DM.map (const DS.empty) prods

  updFirst fstMap = DM.mapWithKey addInner fstMap where
    addInner name vals = let rules = defLookup prods name [] in
      foldr (\(Rule sent _) v -> DS.union v (firstSent fstMap sent)) vals rules

getFollow :: Info -> FFMap -> ProdMap -> FFMap
getFollow info fstMap prods = whileChanges updFollow (addEOI empty (snd (name info))) where
    addEOI m start = DM.insert start (DS.singleton EOI) m
    empty = DM.map (const DS.empty) prods

    updFollow flwMap = foldr updForKids flwMap (DM.keys flwMap)
    updForKids a m = let rules = defLookup prods a [] in foldr (updForRule a) m (map (\(Rule s _) -> s) rules)

    updForRule :: NonTermName -> Sentence -> FFMap -> FFMap
    updForRule _ [] m = m
    updForRule p (Left _ : rest) m = updForRule p rest m
    updForRule parent (Right nTerm : rest) m = updForRule parent rest (DM.insertWith DS.union nTerm flwNTerm m) where
      flwNTerm = let fstRest = firstSent fstMap rest in
        DS.union (DS.delete Eps fstRest) (if DS.member Eps fstRest
                                             then defLookup m parent DS.empty
                                             else DS.empty)


mkParsers :: Info -> [Production] -> Q [Dec]
mkParsers info@(Info (parFunS, parTyS) errFunS tokTyS toks skip) prods
  = do let prodMap = DM.fromList (map (\(Prod n rules) -> (n, rules)) prods)
           fstMap  = getFirst prodMap
           flwMap  = getFollow info fstMap prodMap
           tokName = mkName tokTyS
       decs1 <- mkLexer info
       (parseTyN, decs2) <- mkClassParse tokName
       decs3 <- sequence (map (mkParse parseTyN fstMap flwMap) prods)
       pure $ decs1 ++ decs2 ++ (concat decs3)

mkClassParse :: Name -> Q (Name, [Dec])
mkClassParse tokName
  = do parseTyN <- newName "Parse"
       argTN <- newName "a"
       let tyVar = PlainTV argTN ()
       decs <- sequence [classD (pure []) parseTyN [tyVar] [] [parseFunDec argTN]]
       pure $ (parseTyN, decs)
  where
    parseFunDec argTN
      = do let name = mkName "_parse"
           sigD name (appT (appT arrowT (appT listT (conT tokName)))
                           (appT (appT (tupleT 2) (varT argTN)) (appT listT (conT tokName))))

getFirstSent ffmap sent = convert (DS.toList (firstSent ffmap sent))
  where
    convert [] = []
    convert (Term n:rest) = (Just n):convert rest
    convert (Eps:rest) = Nothing:convert rest
    convert (EOI:rest) = convert rest

getFollowNT flwMap nTName = convert $ DS.toList (defLookup flwMap nTName DS.empty)
  where
    convert [] = []
    convert (Term n:rest) = (Just n):convert rest
    convert (EOI:rest) = Nothing:convert rest
    convert (Eps:rest) = convert rest

mkParse :: Name -> FFMap -> FFMap -> Production -> Q [Dec]
mkParse parseTyN fstMap flwMap (Prod nTStr rules)
  = do let tyName       = mkName nTStr
           instanceType = conT parseTyN `appT` conT tyName
       sequence [instanceD (pure []) instanceType [genParse]]
  where
    genParse = funD (mkName "_parse") [genParseClause rules]

    genParseClause :: [Rule] -> Q Clause
    genParseClause rules
      = do tknsName <- newName "tkns"
           guards <- genGuards tknsName
           pure $ Clause [VarP tknsName] (GuardedB guards) []
      where
        genGuards tkns = do sequence $ (map (genGuard tkns) rules) ++ [otherwiseGuard]
          where
            otherwiseGuard = pure ( NormalG (VarE (mkName "otherwise")),
                                    AppE (VarE (mkName "parseError")) (VarE tkns))
        genGuard tkns (Rule sent block) = do eVal <- expr
                                             pure $ (NormalG guardExp, eVal)
          where
            guardExp = let inFirst = mkName "inFirst"
                           inFollow = mkName "inFollow"
                           fstList = getFirstSent fstMap sent
                           flwList = getFollowNT flwMap nTStr
                           fstChck = AppE (AppE (VarE inFirst) (VarE tkns)) (ListE (map (LitE . StringL) (catMaybes fstList)))
                           flwChck = AppE (AppE (VarE inFollow) (VarE tkns)) (ListE (map maybeToExp flwList))
                       in if elem Nothing fstList
                            then InfixE (Just fstChck) (VarE (mkName "||")) (Just flwChck)
                            else fstChck
              where
                maybeToExp (Just a) = AppE (ConE (mkName "Just")) (LitE (StringL a))
                maybeToExp Nothing = ConE (mkName "Nothing")
            expr = do (kidsNames, tknsRest, decs) <- genDecs
                      let action = getExp block (length sent)
                      let applied = foldr (\kid a -> AppE a (VarE kid)) action kidsNames
                      pure $ LetE (reverse decs) (TupE [Just applied, Just (VarE tknsRest)])
            genDecs :: Q ([Name], Name, [Dec])
            genDecs = foldr (\e b -> do (ks, t, ds) <- b
                                        (k, tn, d) <- genDec e t
                                        pure (k:ks, tn, d:ds))
                            (pure ([], tkns, [])) (reverse sent)
            genDec :: Either String String -> Name -> Q (Name, Name, Dec)
            genDec e tkns = do kid <- newName "kid"
                               tknsNew <- newName "tkns"
                               let dec expr = ValD (TupP [VarP kid, VarP tknsNew]) (NormalB expr) []
                               let expr = case e of
                                          (Right nTerm) -> AppE (VarE (mkName "_parse")) (VarE tkns)
                                          (Left term) -> AppE (AppE (VarE (mkName "consume")) (VarE tkns)) (LitE (StringL term))
                               pure $ (kid, tknsNew, dec expr)

mkLexer :: Info -> Q [Dec]
mkLexer info
  = concat <$> sequence [ mkTokenData info, mkNameFunc info, mkLexerConstants info]

mkLexerConstants :: Info -> Q [Dec]
mkLexerConstants info = concat <$> sequence [ mkRegexs, mkSkip ]
  where
    mkRegexs
      = do let regexsN = mkName "regexs"
           pure $ [ValD (VarP regexsN) (NormalB (ListE getRegexs)) []]
      where
        getRegexs = map getRegex (tokens info)
        getRegex (_, r, b) = TupE [ Just (LitE (StringL ('^':r)))
                                  , Just (getFunc (parseExp b)) ]
        getFunc (AppE c@(ConE _) (ConE _)) = InfixE (Just c) (VarE (mkName ".")) (Just $ VarE (mkName "read"))
        getFunc c@(ConE _) = AppE (VarE (mkName "const")) c
    mkSkip = let skipN = mkName "skip" in pure $ [ValD (VarP skipN) (NormalB (LitE (StringL $ '^':(skip info)))) []]

mkTokenData :: Info -> Q [Dec]
mkTokenData info
  = do let tokTyN = mkName (tokenType info)
           deriv  = [DerivClause Nothing [ConT (mkName "Show")]]
       pure $ [DataD [] tokTyN [] Nothing cons deriv]
  where
    cons = map (\(_, _, x) -> mkCon x) (tokens info)
    mkCon str = let (tokCN, args) = parseConD in
      NormalC tokCN (map (\argTN -> (Bang NoSourceUnpackedness NoSourceStrictness, ConT argTN)) args)
      where
        parseConD = case parseExp str of
          (AppE (ConE tokCN) (ConE argTN)) -> (tokCN, [argTN])
          (ConE tokCN)                     -> (tokCN, [])
          _                                -> error $ "unknown token signature: " ++ str

mkNameFunc :: Info -> Q [Dec]
mkNameFunc info = let tN = mkName "t"
  in pure $ [FunD (mkName "name") [Clause [VarP tN] (NormalB (CaseE (VarE tN) matches)) []]]
  where
    matches = map getMatch (tokens info)
    getMatch (tS, _, tBlock) = Match conPat (NormalB (LitE (StringL tS))) []
      where
        conPat = case parseExp tBlock of
          (AppE (ConE tokCN) (ConE argTN)) -> ConP tokCN [] [WildP]
          (ConE tokCN) -> ConP tokCN [] []