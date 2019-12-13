module Stack (Stack, pop, push, emptyStack) where

import           Control.Monad.State

type Stack a = [a]

pop :: State (Stack a) a
pop = state $ \(x:xs) -> (x, xs)

push :: a -> State (Stack a) ()
push x = state $ \xs -> ((), x:xs)

emptyStack :: Stack a
emptyStack = []
