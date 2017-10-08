module Lang where

type Id = String

data Constant =
  CInt { getInt :: Int }
  | CString { getString :: String }
  | CExtern Id
  | CConstructor Id
  deriving (Show, Eq)

data Pattern =
  PVar Id
  | PConst Constant
  | PApply Pattern [Pattern]
  deriving (Show, Eq)

data Exp =
  EConstant {getConstant :: Constant}
  | EVar Id
  | ELambda [(Pattern, Exp)]
  | EApply Exp [Exp]
  deriving (Show, Eq)
