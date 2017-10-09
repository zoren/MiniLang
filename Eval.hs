module Eval where

import Lang
import Data.Map (Map)
import qualified Data.Map as Map

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
  case (pat, e) of
    (PVar var, _) -> Just $ Map.insert var e env
    (PConst pc, EConstant c) -> if pc == c then Just Map.empty else Nothing
    (PApply pat' pats, EApply e' es) -> bindPattern env e' pat' >>= \env' -> bindPatterns env' es pats
    _ -> error "pattern didn't match expression"

tryEvalFunc :: Environment -> Exp -> [Exp] -> Exp
tryEvalFunc _ f [] = f
tryEvalFunc env f (args@(a:as)) =
  case f of
    ELambda cases ->
      let
        runFirstCase [] = error "pattern not caught"
        runFirstCase ((pat, ecase):cases') =
          maybe (runFirstCase cases') (\newEnv -> tryEvalFunc newEnv (eval newEnv ecase) as) $ bindPattern env a pat
      in
        runFirstCase cases
    EConstant c ->
      case c of
        CExtern extern ->
          maybe (EApply f args) (\(eres, remArgs) -> tryEvalFunc env eres remArgs) $ evalExtern extern args
        CConstructor _ -> EApply f args
        _ -> error "applying constant"
    EApply innerf innerargs -> tryEvalFunc env innerf $ innerargs ++ args
    _ -> error "applying non-function"
      
eval :: Environment -> Exp -> Exp
eval env e =
  case e of
    EVar var -> env Map.! var
    EApply e1 args -> tryEvalFunc env (eval env e1) $ map (eval env) args
    _ -> e

evalEmpty :: Exp -> Exp
evalEmpty = eval Map.empty
