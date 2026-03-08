-- | DEMO 1: Péndulo simple — Ecuaciones de Euler-Lagrange automáticas
-- ================================================================
--
-- Port directo del código scmutils:
--
-- @
-- (define ((L-pendulum m l g) local)
--   (let ((theta     (coordinate local))
--         (theta-dot (velocity local)))
--     (- (* 1/2 m (expt (* l theta-dot) 2))    ; T = ½ m l² θ̇²
--        (* m g l (- 1 (cos theta))))))          ; V = m g l (1 - cos θ)
--
-- (show-expression
--   (((Lagrange-equations (L-pendulum 'm 'l 'g))
--     (literal-function 'theta))
--    't))
-- @
module Main where

import Data.Ratio               ((%))
import Symbolic.Expr
import Symbolic.Simplify        (simplify)
import Mechanics.Lagrangian

-- | Lagrangiano del péndulo simple:
--
--   L = T − V
--   T = ½ m l² θ̇²
--   V = m g l (1 − cos θ)
--
-- Parámetros @m@, @l@, @g@ son simbólicos.
lPendulum :: Expr -> Expr -> Expr -> LocalTuple -> Expr
lPendulum m l g local =
  let theta    = ltCoord    local
      thetaDot = ltVelocity local
      -- T = (1/2) * m * (l * thetaDot)^2
      kineticE = lit (1%2) .* m .* sqrE (l .* thetaDot)
      -- V = m * g * l * (1 - cos(theta))
      potentialE = m .* g .* l .* (num 1 .- cosE theta)
  in  simplify (kineticE .- potentialE)

main :: IO ()
main = do
  putStrLn "====================================================="
  putStrLn " Péndulo simple — Ecuaciones de Euler-Lagrange"
  putStrLn " (port de scmutils a Haskell)"
  putStrLn "====================================================="
  putStrLn ""

  let m = sym "m"
      l = sym "l"
      g = sym "g"

  -- Mostrar el Lagrangiano
  let local = mkLocal "theta"
      lExpr = lPendulum m l g local
  putStrLn "Lagrangiano  L(t, θ, θ̇):"
  putStrLn $ "  " ++ showExpression lExpr
  putStrLn ""

  -- Derivar las ecuaciones de Euler-Lagrange
  let el = lagrangeEquations (lPendulum m l g) "theta"
  putStrLn "Ecuación de Euler-Lagrange  (= 0):"
  putStrLn $ "  " ++ showExpression el
  putStrLn ""
  putStrLn "Es decir:"
  putStrLn "  m*l²*θ̈ + m*g*l*sin(θ) = 0"
  putStrLn ""
  putStrLn "(Dividiendo por m*l se obtiene la ecuación clásica:"
  putStrLn "   l*θ̈ + g*sin(θ) = 0 )"
