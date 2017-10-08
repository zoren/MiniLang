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

tests =
    [onePlusTwo,
    (EApply (EConstant $ CExtern "u-") [EConstant $ CInt 1]),
    (EApply (EConstant $ CExtern "-") [EConstant $ CInt 1, EConstant $ CInt 2]),
    (EApply (EConstant $ CExtern "++") [EConstant $ CString "a", EConstant $ CString "b"]),
    (EApply (ELambda [(PVar "x",
                                    EApply (EConstant $ CExtern "++") [EVar "x", EVar "x"])]) [EConstant $ CString "a"]),
    (EApply (ELambda [(PConst $ CString "a", EConstant $ CInt 1)]) [EConstant $ CString "a"]),
    (EApply caseTest [EConstant $ CString "a"]),
    (EApply caseTest [EConstant $ CString "b"]),
    llist,
    EApply lhead [llist],
    EApply ltail [llist],
    EApply (EApply (EConstant $ CExtern "+") [EConstant $ CInt 1]) [EConstant $ CInt 2],
    -- (\f -> f 2)(u-)
    EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EConstant $ CExtern "u-"],
    -- (\f -> f 2)(+1)
    EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EApply (EConstant $ CExtern "+") [EConstant $ CInt 1]]]
