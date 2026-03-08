-- | Lagrangian mechanics — symbolic Euler-Lagrange equations.
--
-- Follows the scmutils convention where a Lagrangian is a function
-- of a /local tuple/ @(t, q, v)@ — time, generalised coordinate,
-- and generalised velocity.
--
-- A @literal-function@ in scmutils becomes a symbolic function
-- application @theta(t)@ here.
module Mechanics.Lagrangian
  ( LocalTuple(..)
  , mkLocal
  , lagrangeEquations
  , showExpression
  ) where

import Symbolic.Expr
import Symbolic.Differentiation (diff)
import Symbolic.Simplify        (simplify)

-- | The local tuple @(t, q, dq/dt)@, all symbolic.
data LocalTuple = LocalTuple
  { ltTime     :: Expr   -- ^ t
  , ltCoord    :: Expr   -- ^ q(t)   (generalised coordinate)
  , ltVelocity :: Expr   -- ^ dq/dt  (generalised velocity)
  } deriving (Show)

-- | Build the standard local tuple for a single degree of freedom.
--
-- In scmutils, @(literal-function 'theta)@ creates a generic θ(t).
-- Here we represent θ(t) as @Fun "theta" [Sym "t"]@ and θ̇(t) as
-- @Fun "thetadot" [Sym "t"]@ — using an explicit velocity symbol
-- so that partial derivatives ∂L/∂q and ∂L/∂q̇ can be taken
-- independently, exactly as in the variational derivation.
--
-- @coordName@ is the name of the generalised coordinate (e.g. "theta").
mkLocal :: String -> LocalTuple
mkLocal coordName = LocalTuple
  { ltTime     = sym "t"
  , ltCoord    = sym coordName
  , ltVelocity = sym (coordName ++ "dot")
  }

-- | Derive the Euler-Lagrange equation for a single degree of freedom.
--
-- Given a Lagrangian @L(t, q, q̇)@ (represented as an 'Expr' that
-- mentions the symbols @q@ and @qdot@), return the expression that
-- must equal zero:
--
-- @
--   d/dt (∂L/∂q̇) − ∂L/∂q = 0
-- @
--
-- The derivative @d/dt@ is expanded via the chain rule:
--
-- @
--   d/dt F(t, q, q̇) = ∂F/∂t + (∂F/∂q)·q̇ + (∂F/∂q̇)·q̈
-- @
--
-- The result is the left-hand side (should equal 0).
lagrangeEquations
  :: (LocalTuple -> Expr)   -- ^ Lagrangian  L(t, q, q̇)
  -> String                 -- ^ coordinate name (e.g. "theta")
  -> Expr                   -- ^ Euler-Lagrange residual (= 0)
lagrangeEquations lagrangian coordName =
  let
    -- Build local tuple with independent symbols
    local  = mkLocal coordName
    qName  = coordName
    vName  = coordName ++ "dot"
    aName  = coordName ++ "ddot"  -- q̈ (acceleration symbol)

    -- Evaluate L symbolically
    lExpr  = lagrangian local

    -- ∂L/∂q
    dLdq   = diff lExpr qName

    -- ∂L/∂q̇  (partial w.r.t. the velocity symbol)
    dLdv   = diff lExpr vName

    -- Total time derivative  d/dt (∂L/∂q̇)
    -- Using chain rule:
    --   d/dt F = ∂F/∂t + (∂F/∂q)·q̇ + (∂F/∂q̇)·q̈
    ddLdv_dt = simplify $
      diff dLdv "t"
        .+ (diff dLdv qName .* sym vName)
        .+ (diff dLdv vName .* sym aName)

    -- Euler-Lagrange:  d/dt(∂L/∂q̇) − ∂L/∂q = 0
    result = simplify (ddLdv_dt .- dLdq)
  in
    result

-- | Pretty-print an expression (analogous to scmutils @show-expression@).
showExpression :: Expr -> String
showExpression = show
