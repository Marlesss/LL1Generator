module Generator(generate) where

import Grammar
import Lib
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.ParseGen
import qualified Data.Text as T

lineSep :: [String] -> T.Text
lineSep names = T.unlines (map T.pack names)

defaultHeader, importBlock :: [String]
defaultHeader = ["{-# OPTIONS_GHC -w #-}",
                 "{-# LANGUAGE TypeApplications #-}"]

importBlock = [ "import Language.Haskell.TH.ParseGen"
              , "import qualified Data.List as L"
              , "import Unsafe.Coerce(unsafeCoerce)"
              , "import Text.Regex.TDFA"]

generate :: Grammar -> IO T.Text
generate (Grammar h dirs prods t)
  = do generatedBody <- genBody dirs prods
       pure $ mconcat
         [ lineSep defaultHeader
         , T.pack h
         , lineSep importBlock
         , T.pack t
         , generatedBody
         , constBody ]

genBody :: [Directive] -> [Production] -> IO T.Text
genBody dirs prods
  = do let info = (getInfo dirs)
       decs <- TH.runQ $ mkParsers info prods
       let doc = TH.ppr_list decs
       pure $ T.pack $ show doc

getInfo :: [Directive] -> Info
getInfo dirs = let dirsMap = map (\(Directive n args) -> (n, args)) dirs in
  Info
    (getName $ lookup "name" dirsMap)
    (getErrF $ lookup "error" dirsMap)
    (getTokT $ lookup "tokentype" dirsMap)
    (getToks $ lookup "token" dirsMap)
    (getSkip $ lookup "skip" dirsMap)

getName :: Maybe [DirectiveArg] -> String
getName (Just [NonTerm stName]) = stName
getName args = error $ "name undefined" ++ show args

getErrF :: Maybe [DirectiveArg] -> String
getErrF (Just [Term n]) = n
getErrF args = error $ "error undefined: " ++ show args

getTokT :: Maybe [DirectiveArg] -> String
getTokT (Just [Block n]) = n
getTokT args = error $ "tokenType undefined: " ++ show args

getToks :: Maybe [DirectiveArg] -> [(String, String, String)]
getToks (Just []) = []
getToks (Just (Term n : Regex r : Block b : rest)) = (n, r, b) : getToks (Just rest)
getToks args = error $ "tokens undefined: " ++ show args

getSkip :: Maybe [DirectiveArg] -> String
getSkip (Just [Regex r]) = r
getSkip args = error $ "skip undefined: " ++ show args

constBody :: T.Text
constBody = T.pack $ "\n\
\lexer :: String -> [Token]\n\
\lexer [] = []\n\
\lexer str = case L.find ((str =~).fst) regexs of\n\
\ (Just (r, func)) -> let (_, matched, rest) = str =~ r :: (String, String, String)\n\
\                      in func matched : lexer rest\n\
\ Nothing          -> if str =~ skip :: Bool\n\
\                        then let (_, _, rest) = str =~ skip :: (String, String, String)\n\
\                             in lexer rest\n\
\                        else error $ \"unexpected input: \" ++ str\n\
\\n\
\consume a@[] _ = parseError a\n\
\consume (t:rest) s\n\
\  | name t == s = (getVal t, rest)\n\
\  | otherwise = parseError (t:rest)\n\
\\n\
\inFirst :: [Token] -> [String] -> Bool\n\
\inFirst [] _ = False\n\
\inFirst (t:_) fs = elem (name t) fs\n\
\\n\
\inFollow :: [Token] -> [Maybe String] -> Bool\n\
\inFollow [] fs = elem Nothing fs\n\
\inFollow (t:_) fs = elem (Just (name t)) fs\n\
\\n\
\"
