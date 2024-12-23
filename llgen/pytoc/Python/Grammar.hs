module Python.Grammar where

data Program = Program Statement Pf deriving Show
data Pf = PfEmpty | Pf Statement Pf deriving Show

typeToPrimitiveType :: String -> PrimitiveType
typeToPrimitiveType t = case t of
                        "Int"     -> Int
                        "String"  -> String
                        "Boolean" -> Bool

data PrimitiveType
  = Int
  | String
  | Bool
  deriving Show

type VarName = String

data Statement
  = StatementEmpty
  | Define VarName PrimitiveType
  | DefineSet VarName PrimitiveType Expr
  | Set VarName Expr
  | StExpr Expr
  | If Expr Program
  | IfElse Expr Program Program
  | For VarName Range Program
  | While Expr Program
  deriving Show

data Expr = Expr Ar Ef deriving (Show, Eq, Ord)

data Ef
  = EfEmpty
  | LT Ar
  | LE Ar
  | GT Ar
  | GE Ar
  | EQ Ar
  | NQ Ar
  deriving (Show, Eq, Ord)

data Ar = Ar T Arf deriving (Show, Eq, Ord)

data Arf
  = ArfEmpty
  | Plus T Arf
  | Minus T Arf
  deriving (Show, Eq, Ord)

data T = T D Tf deriving (Show, Eq, Ord)

data Tf
  = TfEmpty
  | Times D Tf
  | Div D Tf
  deriving (Show, Eq, Ord)

data D = D C Df deriving (Show, Eq, Ord)

data Df
  = DfEmpty
  | Or C Df
  deriving (Show, Eq, Ord)

data C = C Call Cf deriving (Show, Eq, Ord)

data Cf
  = CfEmpty
  | And Call Cf
  deriving (Show, Eq, Ord)

data Call = Call Y Callf deriving (Show, Eq, Ord)

data Callf
  = CallfEmpty
  | Callf Args
  deriving (Show, Eq, Ord)

data Y
  = Brack Expr
  | IntVal Int
  | StrVal String
  | BoolVal Bool
  | Var VarName
  deriving (Show, Eq, Ord)

data Args
  = ArgsEmpty
  | Args Expr Argsf
  deriving (Show, Eq, Ord)

data Argsf
  = ArgsfEmpty
  | Argsf Expr Argsf
  deriving (Show, Eq, Ord)

data Range
  = Inclusive {from :: Expr, to :: Expr}
  | Exclusive {from :: Expr, to :: Expr}
  deriving (Show, Eq, Ord)