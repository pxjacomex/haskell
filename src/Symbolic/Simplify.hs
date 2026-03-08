-- | Algebraic simplification of symbolic expressions.
--
-- Applies rewrite rules repeatedly until a fixed point is reached.
-- This is intentionally kept simple; a production system would use
-- canonical forms (polynomials, rational functions, etc.).
module Symbolic.Simplify
  ( simplify
  ) where

import Symbolic.Expr

-- | Simplify an expression by applying algebraic identities
-- until no more reductions are possible (fixed-point iteration).
simplify :: Expr -> Expr
simplify e =
  let e' = rewrite (descend e)
  in  if e' == e then e else simplify e'

-- Recursively simplify sub-expressions first (bottom-up).
descend :: Expr -> Expr
descend (Neg a)      = Neg (simplify a)
descend (Add a b)    = Add (simplify a) (simplify b)
descend (Sub a b)    = Sub (simplify a) (simplify b)
descend (Mul a b)    = Mul (simplify a) (simplify b)
descend (Div a b)    = Div (simplify a) (simplify b)
descend (Pow a b)    = Pow (simplify a) (simplify b)
descend (Sin a)      = Sin (simplify a)
descend (Cos a)      = Cos (simplify a)
descend (Deriv a x)  = Deriv (simplify a) x
descend (Fun f args) = Fun f (map simplify args)
descend e            = e

-- One pass of rewrite rules.
rewrite :: Expr -> Expr

-- Double negation
rewrite (Neg (Neg a))          = a

-- Negation of zero
rewrite (Neg (Num 0))          = Num 0

-- Arithmetic on constants
rewrite (Add (Num a) (Num b))  = Num (a + b)
rewrite (Sub (Num a) (Num b))  = Num (a - b)
rewrite (Mul (Num a) (Num b))  = Num (a * b)
rewrite (Div (Num a) (Num b))
  | b /= 0                    = Num (a / b)
rewrite (Neg (Num a))         = Num (negate a)

-- Additive identity
rewrite (Add a (Num 0))       = a
rewrite (Add (Num 0) a)       = a
rewrite (Sub a (Num 0))       = a
rewrite (Sub (Num 0) a)       = Neg a

-- Multiplicative identity / zero
rewrite (Mul _ (Num 0))       = Num 0
rewrite (Mul (Num 0) _)       = Num 0
rewrite (Mul a (Num 1))       = a
rewrite (Mul (Num 1) a)       = a

-- Division identities
rewrite (Div a (Num 1))       = a
rewrite (Div (Num 0) _)       = Num 0

-- Power identities
rewrite (Pow _ (Num 0))       = Num 1
rewrite (Pow a (Num 1))       = a

-- x - x = 0
rewrite (Sub a b) | a == b    = Num 0

-- a/a = 1 (when equal symbolic expressions)
rewrite (Div a b) | a == b    = Num 1

-- sin(0) = 0, cos(0) = 1
rewrite (Sin (Num 0))         = Num 0
rewrite (Cos (Num 0))         = Num 1

-- Pull numeric coefficient to the left: a * k  =>  k * a
-- (helps later simplification stages)
rewrite (Mul a b@(Num _))
  | notNum a                  = Mul b a

-- Combine numeric coefficients:  (k1 * a) * k2  =>  (k1*k2) * a
rewrite (Mul (Num k1) (Mul (Num k2) a)) = Mul (Num (k1 * k2)) a
rewrite (Mul (Mul (Num k) a) b)
  | a == b                    = Mul (Num k) (Pow a (Num 2))

-- Neg as multiplication by -1 for cleaner display
rewrite (Mul (Num (-1)) (Neg a)) = a
rewrite (Neg (Mul (Num k) a))    = Mul (Num (negate k)) a

-- Subtraction to addition of negation (helps canonical form)
-- We keep Sub for display, but simplify Sub a (Neg b) => Add a b
rewrite (Sub a (Neg b))       = Add a b
rewrite (Add a (Neg b))       = Sub a b

-- Default: no rewrite
rewrite e = e

notNum :: Expr -> Bool
notNum (Num _) = False
notNum _       = True
