module Generator where

import Grammar
import Lexer
import Data.Maybe
import Data.List
import Lib
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.ParseGen as PG
import qualified Data.Text as T
import qualified Data.Map as DM
import qualified Data.Set as DS

lineSep :: [String] -> T.Text
lineSep names = T.unlines (map T.pack names)

defaultHeader, importBlock :: [String]
defaultHeader = ["{-# OPTIONS_GHC -w #-}"]

importBlock = [ "import Language.Haskell.TH.ParseGen" ]

generate :: Grammar -> IO T.Text
generate (Grammar h dirs prods t)
  = do body <- genBody dirs prods
       pure $ mconcat
         [ lineSep defaultHeader
         , T.pack h
         , lineSep importBlock
         , T.pack t
         , body ]

genBody :: [Directive] -> [Production] -> IO T.Text
genBody dirs prods
  = do let info = (getInfo dirs)
       decs <- TH.runQ $ PG.mkParsers info prods
       let doc = TH.ppr_list decs
       pure $ T.pack $ show doc

type DirectiveArgs = [Either TermName (Either NonTermName Block)]

getInfo :: [Directive] -> Info
getInfo dirs = let dirsMap = map (\(Directive n args) -> (n, args)) dirs in
  Info
    (getName $ lookup "name" dirsMap)
    (getErrF $ lookup "error" dirsMap)
    (getTokT $ lookup "tokentype" dirsMap)
    (getToks $ lookup "token" dirsMap)

getName :: Maybe DirectiveArgs -> (String, String)
getName (Just [Left fName, Right (Left stName)]) = (fName, stName)
getName _ = ("parse", "Start")

getErrF :: Maybe DirectiveArgs -> String
getErrF (Just [Left n]) = n
getErrF _ = "parseError"

getTokT :: Maybe DirectiveArgs -> String
getTokT (Just [Right (Right n)]) = n
getTokT _ = error "TokenType undefined"

getToks :: Maybe DirectiveArgs -> [(String, String)]
getToks (Just []) = []
getToks (Just (Left n : Right (Right b) : rest)) = (n, b) : getToks (Just rest)
getToks args = error $ "Tokens undefined: " ++ show args


-- TODO: add lexer+Token generation