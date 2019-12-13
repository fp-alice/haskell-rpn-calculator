module Main where

import           Eval
import           System.Environment

main :: IO ()
main = unwords <$> getArgs >>= evalExpr
