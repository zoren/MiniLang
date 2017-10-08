import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)

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

bindPatterns env [] [] = Just env
bindPatterns env (e:es) (p:ps) =
  case bindPattern env e p of
    Nothing -> Nothing
    Just env -> bindPatterns env es ps
bindPatterns _ _ _ = error "apply pattern did not match"

bindPattern :: Map Id Exp -> Exp -> Pattern -> Maybe (Map Id Exp)
bindPattern env e pat =
  case pat of
    PVar id -> Just $ Map.insert id e env
    PConst pc -> if (EConstant pc) == e then Just Map.empty else Nothing
    PApply pat pats ->
      case e of
        EApply e es ->
          case bindPattern env e pat of
            Nothing -> Nothing
            Just env' -> bindPatterns env' es pats
        _ -> error "pattern didn't match expected constructor"

tryEvalFunc env f [] = f
tryEvalFunc env f (args@(a:as)) =
  case f of
    ELambda cases ->
      let
        runFirstCase [] = error "pattern not caught"
        runFirstCase ((pat, ecase):cases') =
          case bindPattern env a pat of
            Nothing -> runFirstCase cases'
            Just newEnv -> tryEvalFunc newEnv (eval newEnv ecase) as
      in
        runFirstCase cases
    EConstant(CExtern extern) ->
      case evalExtern extern args of
        Nothing -> EApply f args
        Just (eres, remArgs) -> tryEvalFunc env eres remArgs
    EConstant(CConstructor id) -> EApply f args
    _ -> error "applying non-function"
      
eval env e =
  case e of
    EConstant _ -> e
    ELambda _ -> e
    EVar id -> env Map.! id
    EApply e1 args -> tryEvalFunc env (eval env e1) $ map (eval env) args

onePlusTwo = EApply (EConstant $ CExtern "+") [EConstant $ CInt 1, EConstant $ CInt 2]

caseTest =
  (ELambda [(PConst $ CString "a", EConstant $ CInt 1),
                   (PConst $ CString "b", EConstant $ CInt 2)])

lhead = ELambda [(PApply (PConst $ CConstructor "cons") [PVar "h", PVar "t"], EVar "h")]
ltail = ELambda [(PApply (PConst $ CConstructor "cons") [PVar "h", PVar "t"], EVar "t")]

llist = (EApply (EConstant $ CConstructor "cons") [onePlusTwo, EConstant $ CConstructor "nil"])

main =
  do
    print $ eval Map.empty onePlusTwo
    print $ eval Map.empty (EApply (EConstant $ CExtern "u-") [EConstant $ CInt 1]) 
    print $ eval Map.empty (EApply (EConstant $ CExtern "-") [EConstant $ CInt 1, EConstant $ CInt 2])
    print $ eval Map.empty (EApply (EConstant $ CExtern "++") [EConstant $ CString "a", EConstant $ CString "b"])
    print $ eval Map.empty (EApply (ELambda [(PVar "x",
                                    EApply (EConstant $ CExtern "++") [EVar "x", EVar "x"])]) [EConstant $ CString "a"])
    print $ eval Map.empty (EApply (ELambda [(PConst $ CString "a", EConstant $ CInt 1)]) [EConstant $ CString "a"])
    print $ eval Map.empty (EApply caseTest [EConstant $ CString "a"])
    print $ eval Map.empty (EApply caseTest [EConstant $ CString "b"])
    print $ eval Map.empty llist
    print $ eval Map.empty $ EApply lhead [llist]
    print $ eval Map.empty $ EApply ltail [llist]
    -- (\f -> f 2)(u-)
    print $ eval Map.empty $ EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EConstant $ CExtern "u-"]
