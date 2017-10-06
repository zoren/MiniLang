import Data.Map (Map)
import qualified Data.Map as Map

type Id = String

data Constant =
  CInt { getInt :: Int }
  | CString { getString :: String }
  | CExtern Id
  deriving (Show, Eq)

data Exp =
  EConstant {getConstant :: Constant}
  | EVar Id
  | ELambda Id Exp
  | EApply Exp [Exp]
  deriving (Show, Eq)

trySplitAt n l =
  if length l >= n
  then Just $ splitAt n l
  else Nothing

apply2 ul l f a1 a2 = l $ f (ul a1) (ul a2)

applyConstant = apply2 getConstant EConstant

aII = applyConstant . apply2 getInt CInt
aSS = applyConstant . apply2 getString CString

uminus a = EConstant $ CInt $ - (getInt . getConstant) a

tryApply1 _ [] = Nothing
tryApply1 f (x:l) = Just (f x, l)

tryApply2 _ [] = Nothing
tryApply2 _ [_] = Nothing
tryApply2 f (x:y:l) = Just (f x y, l)
  
evalExtern id =
  case id of
    "+" -> tryApply2 (aII (+))
    "u-" -> tryApply1 uminus
    "-" -> tryApply2 (aII (-))
    "++" -> tryApply2 (aSS (++))
    _ -> error $ "External function not defined: " ++ id    
      
eval env e =
  case e of
    EConstant _ -> e
    ELambda _ _ -> e
    EVar id -> env Map.! id
    EApply e1 args ->
      let vargs = map (eval env) args in
      case eval env e1 of
        EConstant(CExtern extern) ->
          case evalExtern extern vargs of
            Nothing -> e
            Just (eres, remArgs) ->
              eval env $
              if remArgs == []
              then eres
              else EApply eres remArgs
        _ -> error "applying non-function"
           
main =
  do
    print $ eval Map.empty (EApply (EConstant $ CExtern "+") [EConstant $ CInt 1, EConstant $ CInt 2])
    print $ eval Map.empty (EApply (EConstant $ CExtern "u-") [EConstant $ CInt 1]) 
    print $ eval Map.empty (EApply (EConstant $ CExtern "-") [EConstant $ CInt 1, EConstant $ CInt 2])
    print $ eval Map.empty (EApply (EConstant $ CExtern "++") [EConstant $ CString "a", EConstant $ CString "b"])   
