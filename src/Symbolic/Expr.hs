-- | Symbolic expression trees for computer algebra.
--
-- Mirrors the symbolic layer of MIT scmutils: every quantity is an
-- unevaluated expression tree that can be inspected, differentiated,
-- and pretty-printed.
module Symbolic.Expr
  ( Expr(..)
  , sym, num, lit
  , (.+), (.-), (.*), (./), (.^)
  , negE, sinE, cosE, sqrE
  , funApp
  , isZero, isOne
  , freeVars
  ) where

import Data.Ratio (numerator, denominator)
import Data.List  (nub)

-- | Core symbolic expression type.
data Expr
  = Num Rational            -- ^ Literal number (exact rational)
  | Sym String              -- ^ Named symbol  (e.g. \"theta\", \"m\")
  | Neg Expr               -- ^ Unary negation
  | Add Expr Expr          -- ^ Addition
  | Sub Expr Expr          -- ^ Subtraction
  | Mul Expr Expr          -- ^ Multiplication
  | Div Expr Expr          -- ^ Division
  | Pow Expr Expr          -- ^ Exponentiation
  | Sin Expr              -- ^ sin(·)
  | Cos Expr              -- ^ cos(·)
  | Fun String [Expr]     -- ^ Generic function application  f(args…)
  | Deriv Expr String     -- ^ Derivative  de/dx  (kept symbolic when
                          --   it cannot be resolved)
  deriving (Eq, Ord)

-- -----------------------------------------------------------------
-- Smart constructors
-- -----------------------------------------------------------------

sym :: String -> Expr
sym = Sym

num :: Integer -> Expr
num n = Num (fromInteger n)

lit :: Rational -> Expr
lit = Num

funApp :: String -> [Expr] -> Expr
funApp = Fun

-- -----------------------------------------------------------------
-- Infix operators (to avoid clash with Prelude)
-- -----------------------------------------------------------------

infixl 6 .+, .-
infixl 7 .*, ./
infixr 8 .^

(.+), (.-), (.*), (./) :: Expr -> Expr -> Expr
(.+) = Add
(.-) = Sub
(.*) = Mul
(./) = Div

(.^) :: Expr -> Expr -> Expr
(.^) = Pow

negE :: Expr -> Expr
negE = Neg

sinE, cosE :: Expr -> Expr
sinE = Sin
cosE = Cos

sqrE :: Expr -> Expr
sqrE e = Pow e (Num 2)

-- -----------------------------------------------------------------
-- Predicates
-- -----------------------------------------------------------------

isZero :: Expr -> Bool
isZero (Num 0) = True
isZero _       = False

isOne :: Expr -> Bool
isOne (Num 1) = True
isOne _       = False

-- -----------------------------------------------------------------
-- Free variables
-- -----------------------------------------------------------------

freeVars :: Expr -> [String]
freeVars = nub . go
  where
    go (Num _)      = []
    go (Sym s)      = [s]
    go (Neg a)      = go a
    go (Add a b)    = go a ++ go b
    go (Sub a b)    = go a ++ go b
    go (Mul a b)    = go a ++ go b
    go (Div a b)    = go a ++ go b
    go (Pow a b)    = go a ++ go b
    go (Sin a)      = go a
    go (Cos a)      = go a
    go (Fun _ args) = concatMap go args
    go (Deriv a _)  = go a

-- -----------------------------------------------------------------
-- Pretty-printing
-- -----------------------------------------------------------------

instance Show Expr where
  showsPrec = showExpr

showExpr :: Int -> Expr -> ShowS
showExpr _ (Num r)
  | denominator r == 1 = shows (numerator r)
  | otherwise          = showParen True $
      shows (numerator r) . showChar '/' . shows (denominator r)

showExpr _ (Sym s) = showString s

showExpr d (Neg a) = showParen (d > 6) $
  showChar '-' . showExpr 7 a

showExpr d (Add a b) = showParen (d > 6) $
  showExpr 6 a . showString " + " . showExpr 7 b

showExpr d (Sub a b) = showParen (d > 6) $
  showExpr 6 a . showString " - " . showExpr 7 b

showExpr d (Mul a b) = showParen (d > 7) $
  showExpr 7 a . showString "*" . showExpr 8 b

showExpr d (Div a b) = showParen (d > 7) $
  showExpr 7 a . showString "/" . showExpr 8 b

showExpr _ (Pow a b) =
  showExpr 9 a . showString "^" . showExpr 9 b

showExpr _ (Sin a) =
  showString "sin(" . showExpr 0 a . showChar ')'

showExpr _ (Cos a) =
  showString "cos(" . showExpr 0 a . showChar ')'

showExpr _ (Fun f args) =
  showString f . showChar '('
  . showArgs (map (showExpr 0) args)
  . showChar ')'

showExpr d (Deriv e x) = showParen (d > 0) $
  showString "D[" . showExpr 0 e . showString ", " . showString x . showChar ']'

showArgs :: [ShowS] -> ShowS
showArgs []     = id
showArgs [x]    = x
showArgs (x:xs) = x . showString ", " . showArgs xs
