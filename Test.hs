module Test where

import Lang
import Eval
import qualified Data.Map as Map

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
