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

app2 ul1 ul2 f a1 a2 = f (ul1 a1) (ul2 a2)
apply2 ul l f a1 a2 = l $ app2 ul ul f a1 a2

data Mapper a b = M{up:: a -> b, down:: b -> a}
flipMapper M{up=u, down=d}=M{up=d, down=u}

compose :: Mapper a b -> Mapper b c -> Mapper a c
compose M{up=u1, down=d1}  M{up=u2, down=d2} = M{up= u2 . u1, down = d1 . d2}

func :: Mapper a b -> Mapper c d -> Mapper (a -> c) (b -> d)
func (M{up=u1, down=d1}) (M{up=u2,down=d2}) = M{up = \f -> u2 . f . d1, down = \f -> d2 . f . u1}

tup2 :: Mapper a b -> Mapper c d -> Mapper (a, c) (b, d)
tup2 (M{up=u1,down=d1}) (M{up=u2,down=d2}) = M{up = \(a, c) -> (u1 a, u2 c), down = \(b,d) -> (d1 b, d2 d)}

intMap = M{up = getInt, down = CInt}
stringMap = M{up = getString, down = CString}

constMap = M{up = getConstant, down = EConstant}

ciMap = compose constMap intMap
csMap = compose constMap stringMap

aII = down $ func ciMap (func ciMap ciMap)
aSS = down $ func csMap (func csMap csMap)

i2i = func ciMap ciMap

tryApply1 _ [] = Nothing
tryApply1 f (x:l) = Just (f x, l)

tryApply2 _ [] = Nothing
tryApply2 _ [_] = Nothing
tryApply2 f (x:y:l) = Just (f x y, l)
  
evalExtern id =
  case id of
    "+" -> tryApply2 (aII (+))
    "u-" -> tryApply1 $ down i2i (\x -> -x)
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
        ELambda var ebody ->
          foldl (\f a -> eval (Map.insert var a env) f) ebody vargs
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
