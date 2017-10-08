module Main where

import Eval
import Test

main :: IO ()
main = mapM_ print $ map (\e -> evalEmpty e) tests
