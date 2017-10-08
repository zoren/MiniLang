import Data.Map (Map)
import qualified Data.Map as Map

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

data Mapper a b = M{lift:: b -> a, unlift:: a -> b }

compose :: Mapper a b -> Mapper b c -> Mapper a c
compose M{lift = l1, unlift = u1}  M{lift = l2, unlift = u2} = M{lift = l1 . l2, unlift = u2 . u1}

infixr -->
(-->) :: Mapper a b -> Mapper c d -> Mapper (a -> c) (b -> d)
(-->) M{lift = l1, unlift = u1} M{lift = l2, unlift = u2} = M{lift = \f -> l2 . f . u1, unlift = \f -> u2 . f . l1}

evalExtern :: Id -> [Exp] -> Maybe (Exp, [Exp])
evalExtern externalSymbol =
  let
    constMap = M{unlift = getConstant, lift = EConstant}

    int = compose constMap M{lift = CInt, unlift = getInt}
    string = compose constMap M{lift = CString, unlift = getString}

    ii2i = int -->  int --> int
    ss2s = string --> string --> string

    i2i = int --> int
  
    tryApply1 _ [] = Nothing
    tryApply1 f (x:l) = Just (f x, l)

    tryApply2 _ [] = Nothing
    tryApply2 _ [_] = Nothing
    tryApply2 f (x:y:l) = Just (f x y, l)
  in
    case externalSymbol of
      "+" -> tryApply2 $ lift ii2i (+)
      "u-" -> tryApply1 $ lift i2i (\x -> -x)
      "-" -> tryApply2 $ lift ii2i (-)
      "++" -> tryApply2 $ lift ss2s (++)
      _ -> error $ "External function not defined: " ++ externalSymbol

type Environment = Map Id Exp

bindPatterns :: Environment -> [Exp] -> [Pattern] -> Maybe Environment
bindPatterns env [] [] = Just env
bindPatterns env (e:es) (p:ps) = bindPattern env e p >>= \newEnv -> bindPatterns newEnv es ps
bindPatterns _ _ _ = error "apply pattern did not match"

bindPattern :: Map Id Exp -> Exp -> Pattern -> Maybe (Map Id Exp)
bindPattern env e pat =
  case pat of
    PVar var -> Just $ Map.insert var e env
    PConst pc -> if pc == getConstant e then Just Map.empty else Nothing
    PApply pat' pats ->
      case e of
        EApply e' es -> bindPattern env e' pat' >>= \env' -> bindPatterns env' es pats
        _ -> error "pattern didn't match expected constructor"

tryEvalFunc :: Environment -> Exp -> [Exp] -> Exp
tryEvalFunc _ f [] = f
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
    EConstant(CConstructor _) -> EApply f args
    EApply innerf innerargs -> tryEvalFunc env innerf $ innerargs ++ args
    _ -> error "applying non-function"
      
eval :: Environment -> Exp -> Exp
eval env e =
  case e of
    EConstant _ -> e
    ELambda _ -> e
    EVar var -> env Map.! var
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
    print $ eval Map.empty $ EApply (EApply (EConstant $ CExtern "+") [EConstant $ CInt 1]) [EConstant $ CInt 2]
    -- (\f -> f 2)(u-)
    print $ eval Map.empty $ EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EConstant $ CExtern "u-"]
    -- (\f -> f 2)(+1)
    print $ eval Map.empty $ EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EApply (EConstant $ CExtern "+") [EConstant $ CInt 1]]
