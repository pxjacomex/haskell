-- | Hamiltonian mechanics via Legendre transformation.
--
-- Given a Lagrangian L(t, q₁,…,qₙ, q̇₁,…,q̇ₙ), computes:
--
--   * Conjugate momenta   pᵢ = ∂L/∂q̇ᵢ
--   * Hamiltonian          H  = Σ q̇ᵢ · pᵢ  −  L
--
-- The result is expressed in terms of the original coordinates and
-- velocities (no inversion is attempted).
module Mechanics.Hamiltonian
  ( conjugateMomenta
  , legendreTransform
  ) where

import Symbolic.Expr
import Symbolic.Differentiation (diff)
import Symbolic.Simplify        (simplify)

-- | Compute conjugate momenta  pᵢ = ∂L/∂q̇ᵢ  for each velocity symbol.
--
-- Returns a list of @(velocityName, momentumExpr)@ pairs.
conjugateMomenta :: Expr -> [String] -> [(String, Expr)]
conjugateMomenta lagr velNames =
  [ (vName, diff lagr vName)
  | vName <- velNames
  ]

-- | Legendre transform of a Lagrangian.
--
-- @
--   H = Σᵢ q̇ᵢ · (∂L/∂q̇ᵢ)  −  L
-- @
--
-- The velocity symbols are given as a list of names (e.g.
-- @[\"theta1dot\", \"theta2dot\"]@).
legendreTransform :: Expr -> [String] -> Expr
legendreTransform lagr velNames =
  let momenta = conjugateMomenta lagr velNames
      -- Σ q̇ᵢ · pᵢ
      sumPV   = foldl (.+) (num 0)
                  [ sym vName .* pExpr | (vName, pExpr) <- momenta ]
  in  simplify (sumPV .- lagr)
