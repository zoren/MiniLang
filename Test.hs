module Test where

import Lang
import Eval
import qualified Data.Map as Map

onePlusTwo = EApply (EExtern "+") [EConstant $ CInt 1, EConstant $ CInt 2]

caseTest =
  (ELambda [(PConst $ CString "a", EConstant $ CInt 1),
                   (PConst $ CString "b", EConstant $ CInt 2)])

lhead = ELambda [(PApply (PConst $ CConstructor "cons") [PVar "h", PVar "t"], EVar "h")]
ltail = ELambda [(PApply (PConst $ CConstructor "cons") [PVar "h", PVar "t"], EVar "t")]

llist = (EApply (EConstant $ CConstructor "cons") [onePlusTwo, EConstant $ CConstructor "nil"])

tests =
    [onePlusTwo,
    (EApply (EExtern "u-") [EConstant $ CInt 1]),
    (EApply (EExtern "-") [EConstant $ CInt 1, EConstant $ CInt 2]),
    (EApply (EExtern "++") [EConstant $ CString "a", EConstant $ CString "b"]),
    (EApply (ELambda [(PVar "x",
                                    EApply (EExtern "++") [EVar "x", EVar "x"])]) [EConstant $ CString "a"]),
    (EApply (ELambda [(PConst $ CString "a", EConstant $ CInt 1)]) [EConstant $ CString "a"]),
    (EApply caseTest [EConstant $ CString "a"]),
    (EApply caseTest [EConstant $ CString "b"]),
    llist,
    EApply lhead [llist],
    EApply ltail [llist],
    EApply (EApply (EExtern "+") [EConstant $ CInt 1]) [EConstant $ CInt 2],
    -- (\f -> f 2)(u-)
    EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EExtern "u-"],
    -- (\f -> f 2)(+1)
    EApply (ELambda [(PVar "f", EApply (EVar "f") [EConstant $ CInt 2])]) [EApply (EExtern "+") [EConstant $ CInt 1]]]
