module Lib
    ( Info(..)
    ) where

data Info = Info
  { name :: (String, String)
  , errF :: String
  , tokenType :: String
  , tokens :: [(String, String, String)]
  , skip :: String
  } deriving Show
