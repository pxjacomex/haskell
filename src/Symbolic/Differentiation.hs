-- | Symbolic differentiation engine.
--
-- Implements the standard rules (sum, product, chain, …) and
-- leaves derivatives of opaque functions in 'Deriv' form,
-- exactly like scmutils does with @(D f)@.
module Symbolic.Differentiation
  ( diff
  , diffN
  ) where

import Symbolic.Expr
import Symbolic.Simplify (simplify)

-- | @diff e x@ — symbolic derivative of @e@ with respect to
-- the variable named @x@.  The result is simplified.
diff :: Expr -> String -> Expr
diff e x = simplify (rawDiff e x)

-- | @diffN n e x@ — nth derivative.
diffN :: Int -> Expr -> String -> Expr
diffN 0 e _ = e
diffN n e x = diffN (n - 1) (diff e x) x

-- Raw (unsimplified) differentiation.
rawDiff :: Expr -> String -> Expr

-- Constants
rawDiff (Num _) _ = Num 0

-- Variable
rawDiff (Sym s) x
  | s == x    = Num 1
  | otherwise = Num 0

-- Linearity
rawDiff (Neg a) x = Neg (rawDiff a x)

rawDiff (Add a b) x = Add (rawDiff a x) (rawDiff b x)
rawDiff (Sub a b) x = Sub (rawDiff a x) (rawDiff b x)

-- Product rule:  (a*b)' = a'*b + a*b'
rawDiff (Mul a b) x =
  Add (Mul (rawDiff a x) b)
      (Mul a (rawDiff b x))

-- Quotient rule:  (a/b)' = (a'*b - a*b') / b²
rawDiff (Div a b) x =
  Div (Sub (Mul (rawDiff a x) b)
           (Mul a (rawDiff b x)))
      (Pow b (Num 2))

-- Power rule + chain rule:
--   d/dx [f^n]  = n * f^(n-1) * f'          (n constant)
--   d/dx [a^g]  = a^g * (g' * ln a + g * a'/a)  (general, but we
--     only handle the constant-exponent case for simplicity)
rawDiff (Pow base (Num n)) x =
  Mul (Mul (Num n) (Pow base (Num (n - 1))))
      (rawDiff base x)
rawDiff p@(Pow _ _) x = Deriv p x    -- leave as-is for non-constant exp

-- Trig — chain rule
rawDiff (Sin a) x = Mul (Cos a)           (rawDiff a x)
rawDiff (Cos a) x = Neg (Mul (Sin a)      (rawDiff a x))

-- Generic function: leave derivative symbolic.
-- D[f(args), x]  —  if f depends on x only through its arguments,
-- apply the chain rule with a symbolic D[f].
--
-- For a single-argument function f(u):
--   d/dx f(u) = Df(u) * du/dx
rawDiff (Fun f [u]) x =
  Mul (Fun ("D" ++ f) [u]) (rawDiff u x)

-- Multi-argument: produce partial derivatives (sum of chains).
rawDiff (Fun f args) x =
  foldl Add (Num 0)
    [ Mul (Fun ("D_" ++ show i ++ " " ++ f) args)
          (rawDiff ai x)
    | (i, ai) <- zip [(0::Int)..] args
    ]

-- Already-symbolic derivative: nest
rawDiff d@(Deriv _ _) x = Deriv d x
