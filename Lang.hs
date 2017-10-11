module Lang where

type Id = String

data Constant =
  CInt { getInt :: Int }
  | CString { getString :: String }
  | CConstructor Id
  deriving (Show, Eq)

data Pattern =
  PVar Id
  | PConst Constant
  | PApply Pattern [Pattern]
  deriving (Show, Eq)

data Exp =
  EConstant {getConstant :: Constant}
  | EExtern Id
  | EVar Id
  | ELambda [(Pattern, Exp)]
  | EApply Exp [Exp]
  deriving (Show, Eq)
