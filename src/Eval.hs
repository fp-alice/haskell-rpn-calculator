module Eval where

import           Control.Monad.State
import           Parser
import           Stack

eval :: Expr -> State (Stack Int) ()
eval (Int x) = modify (x :)
eval Add     = modify (pure . sum)
eval Sub     = modify (pure . foldl1 (-))
eval Mul     = modify (pure . product)
eval Div     = modify (pure . foldl1 div)

run :: [Expr] -> Int
run s = case result of
  [x] -> x
  _   -> 0
  where
    stack = traverse eval s
    result = evalState (stack >> get) []

evalExpr :: String -> IO ()
evalExpr s = case readExpr s of
  Right ex -> print $ show (run ex) ++ " : " ++ (unwords . fmap show) ex
  Left  er -> print er
