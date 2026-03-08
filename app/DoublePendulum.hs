-- | DEMO 2: Péndulo doble — Lagrangiano y Hamiltoniano simbólicos
-- ================================================================
--
-- Sistema de péndulo doble con 5 parámetros simbólicos:
--   m1, m2  — masas de los dos péndulos
--   l1, l2  — longitudes de las varillas
--   g       — aceleración gravitatoria
--
-- Coordenadas generalizadas: θ₁, θ₂
--
-- Se calcula:
--   1. El Lagrangiano  L = T − V
--   2. Los momentos conjugados  pᵢ = ∂L/∂θ̇ᵢ
--   3. El Hamiltoniano  H = Σ θ̇ᵢ · pᵢ − L  (transformada de Legendre)
module Main where

import Data.Ratio                ((%))
import Symbolic.Expr
import Symbolic.Simplify         (simplify)
import Mechanics.Lagrangian      (showExpression)
import Mechanics.Hamiltonian     (conjugateMomenta, legendreTransform)

-- | Lagrangiano del péndulo doble.
--
-- Posiciones (desde el pivote, eje y hacia abajo):
--
--   x₁ =  l₁ sin θ₁                y₁ = −l₁ cos θ₁
--   x₂ =  l₁ sin θ₁ + l₂ sin θ₂   y₂ = −l₁ cos θ₁ − l₂ cos θ₂
--
-- Energía cinética:
--
--   T₁ = ½ m₁ l₁² θ̇₁²
--   T₂ = ½ m₂ [ l₁² θ̇₁² + l₂² θ̇₂² + 2 l₁ l₂ θ̇₁ θ̇₂ cos(θ₁ − θ₂) ]
--
-- Energía potencial (con referencia en el pivote):
--
--   V₁ = −m₁ g l₁ cos θ₁
--   V₂ = −m₂ g (l₁ cos θ₁ + l₂ cos θ₂)
--
lDoublePendulum
  :: Expr -> Expr -> Expr -> Expr -> Expr  -- m1, m2, l1, l2, g
  -> Expr                                   -- resultado: L
lDoublePendulum m1 m2 l1 l2 g =
  let theta1    = sym "theta1"
      theta2    = sym "theta2"
      theta1dot = sym "theta1dot"
      theta2dot = sym "theta2dot"

      half = lit (1 % 2)

      -- T₁ = ½ m₁ l₁² θ̇₁²
      t1 = half .* m1 .* sqrE l1 .* sqrE theta1dot

      -- T₂ = ½ m₂ (l₁² θ̇₁² + l₂² θ̇₂² + 2 l₁ l₂ θ̇₁ θ̇₂ cos(θ₁−θ₂))
      t2 = half .* m2
           .* ( sqrE l1 .* sqrE theta1dot
                .+ sqrE l2 .* sqrE theta2dot
                .+ num 2 .* l1 .* l2 .* theta1dot .* theta2dot
                   .* cosE (theta1 .- theta2)
              )

      -- V₁ = −m₁ g l₁ cos θ₁
      v1 = negE (m1 .* g .* l1 .* cosE theta1)

      -- V₂ = −m₂ g (l₁ cos θ₁ + l₂ cos θ₂)
      v2 = negE (m2 .* g .* (l1 .* cosE theta1 .+ l2 .* cosE theta2))

      -- L = T − V = (T₁ + T₂) − (V₁ + V₂)
  in  simplify ((t1 .+ t2) .- (v1 .+ v2))

main :: IO ()
main = do
  putStrLn "====================================================="
  putStrLn " Péndulo doble — Lagrangiano y Hamiltoniano"
  putStrLn " 5 parámetros simbólicos: m1, m2, l1, l2, g"
  putStrLn "====================================================="
  putStrLn ""

  let m1 = sym "m1"
      m2 = sym "m2"
      l1 = sym "l1"
      l2 = sym "l2"
      g  = sym "g"

  -- 1. Lagrangiano
  let lagr = lDoublePendulum m1 m2 l1 l2 g
  putStrLn "Lagrangiano  L(θ₁, θ₂, θ̇₁, θ̇₂):"
  putStrLn $ "  " ++ showExpression lagr
  putStrLn ""

  -- 2. Momentos conjugados
  let velNames = ["theta1dot", "theta2dot"]
      momenta  = conjugateMomenta lagr velNames
  putStrLn "Momentos conjugados:"
  mapM_ (\(vName, pExpr) -> do
      let coordName = take (length vName - 3) vName  -- remove "dot"
      putStrLn $ "  p_" ++ coordName ++ " = ∂L/∂" ++ vName ++ " ="
      putStrLn $ "    " ++ showExpression pExpr
    ) momenta
  putStrLn ""

  -- 3. Hamiltoniano (transformada de Legendre)
  let hamiltonian = legendreTransform lagr velNames
  putStrLn "Hamiltoniano  H = Σ θ̇ᵢ·pᵢ − L  (en coordenadas y velocidades):"
  putStrLn $ "  " ++ showExpression hamiltonian
  putStrLn ""
  putStrLn "(El Hamiltoniano se expresa en función de θ₁, θ₂, θ̇₁, θ̇₂."
  putStrLn " Para la forma canónica H(q,p) se requiere invertir"
  putStrLn " pᵢ = ∂L/∂θ̇ᵢ y sustituir las velocidades por los momentos.)"
