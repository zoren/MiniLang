type Id = Text

data Constant =
  CInt Int
  | CString Text
  | CExtern Id

data Exp =
  EConstant Constant
  | EVar Id
  | ELambda Id Exp
  | EApply Exp [Exp]

