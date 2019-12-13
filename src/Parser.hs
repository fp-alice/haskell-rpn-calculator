module Parser (readExpr, Expr(..)) where

import           Data.Either.Combinators
import           Data.Foldable
import           Text.ParserCombinators.Parsec

data Expr = Int Int
          | Add
          | Mul
          | Div
          | Sub

instance Show Expr where
  show (Int x) = show x
  show Add     = "+"
  show Mul     = "*"
  show Div     = "/"
  show Sub     = "-"

parseInt :: Parser Expr
parseInt = Int . read <$> many1 digit

parseOp :: a -> Char -> Parser a
parseOp e c = e <$ char c

parseOperator :: Parser Expr
parseOperator = optional spaces *> parseOperators [(Add, '+'), (Sub, '-'), (Mul, '*'), (Div, '/')]
  where parseOperators = asum . fmap (uncurry parseOp)

parseExpr :: Parser [Expr]
parseExpr = sepBy (parseOperator <|> parseInt) spaces

readExpr :: String -> Either String [Expr]
readExpr input = mapLeft show result
  where result = parse parseExpr "rpn" input
