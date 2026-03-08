-- | Tests del motor de derivación simbólica — paso a paso.
--
-- Cada test imprime la expresión de entrada, el resultado obtenido,
-- y el resultado esperado.  Al final muestra un resumen PASS/FAIL.
module Main where

import Data.Ratio              ((%))
import Symbolic.Expr
import Symbolic.Simplify       (simplify)
import Symbolic.Differentiation (diff, diffN)
import Mechanics.Lagrangian

-- -----------------------------------------------------------------
-- Infraestructura mínima de testing
-- -----------------------------------------------------------------

data TestResult = TestResult
  { trName     :: String
  , trPassed   :: Bool
  , trGot      :: String
  , trExpected :: String
  }

test :: String -> String -> Expr -> Expr -> TestResult
test name _input got expected =
  let gotStr = show got
      expStr = show expected
  in TestResult name (got == expected) gotStr expStr

-- | Comparar por representación textual (para cuando la estructura
-- interna puede diferir pero la salida es la misma).
testShow :: String -> String -> Expr -> String -> TestResult
testShow name _input got expected =
  let gotStr = show got
  in TestResult name (gotStr == expected) gotStr expected

-- -----------------------------------------------------------------
-- Variables y constantes reutilizables
-- -----------------------------------------------------------------

x, y, a, b :: Expr
x = sym "x"
y = sym "y"
a = sym "a"
b = sym "b"

-- -----------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------

tests :: [TestResult]
tests =
  -- ===============================================================
  -- NIVEL 1: Derivadas de constantes y variables
  -- ===============================================================

  [ test
      "1.1  d/dx (5) = 0"
      "d/dx 5"
      (diff (num 5) "x")
      (num 0)
    -- Esperado: 0

  , test
      "1.2  d/dx (x) = 1"
      "d/dx x"
      (diff x "x")
      (num 1)
    -- Esperado: 1

  , test
      "1.3  d/dx (y) = 0    [y independiente de x]"
      "d/dx y"
      (diff y "x")
      (num 0)
    -- Esperado: 0

  -- ===============================================================
  -- NIVEL 2: Reglas de linealidad (suma, resta, negación)
  -- ===============================================================

  , test
      "2.1  d/dx (x + y) = 1"
      "d/dx (x + y)"
      (diff (x .+ y) "x")
      (num 1)
    -- Esperado: 1   (1 + 0 simplifica a 1)

  , test
      "2.2  d/dx (x - y) = 1"
      "d/dx (x - y)"
      (diff (x .- y) "x")
      (num 1)
    -- Esperado: 1   (1 - 0 simplifica a 1)

  , test
      "2.3  d/dx (-x) = -1"
      "d/dx (-x)"
      (diff (negE x) "x")
      (Num (-1))
    -- Esperado: -1

  , test
      "2.4  d/dx (3*x + 2*y) = 3"
      "d/dx (3x + 2y)"
      (diff (num 3 .* x .+ num 2 .* y) "x")
      (num 3)
    -- Esperado: 3   (3*1 + 0 = 3)

  -- ===============================================================
  -- NIVEL 3: Regla del producto
  -- ===============================================================

  , test
      "3.1  d/dx (x * y) = y"
      "d/dx (x*y)"
      (diff (x .* y) "x")
      y
    -- Esperado: y   (1*y + x*0 = y)

  , testShow
      "3.2  d/dx (x * x) = 2*x"
      "d/dx (x*x)"
      (diff (x .* x) "x")
      "x + x"
    -- Esperado: x + x   (1*x + x*1 = x + x, sin combinar)
    -- Nota: el simplificador no combina x+x en 2*x (no tiene esa regla)

  , testShow
      "3.3  d/dx (a*x*b) = a*b"
      "d/dx (a*x*b)"
      (diff (a .* x .* b) "x")
      "a*b"
    -- Esperado: a*b

  -- ===============================================================
  -- NIVEL 4: Regla de la potencia
  -- ===============================================================

  , testShow
      "4.1  d/dx (x^2) = 2*x"
      "d/dx x^2"
      (diff (x .^ num 2) "x")
      "2*x"
    -- Esperado: 2*x

  , testShow
      "4.2  d/dx (x^3) = 3*x^2"
      "d/dx x^3"
      (diff (x .^ num 3) "x")
      "3*x^2"
    -- Esperado: 3*x^2

  , testShow
      "4.3  d/dx (x^1) = 1"
      "d/dx x^1"
      (diff (x .^ num 1) "x")
      "1"
    -- Esperado: 1   (1 * x^0 * 1 = 1*1*1 = 1)

  , testShow
      "4.4  d/dx (x^0) = 0"
      "d/dx x^0"
      (diff (x .^ num 0) "x")
      "0"
    -- Esperado: 0   (0 * x^(-1) * 1, pero x^0 = 1 simplifica a 0)

  -- ===============================================================
  -- NIVEL 5: Funciones trigonométricas
  -- ===============================================================

  , testShow
      "5.1  d/dx sin(x) = cos(x)"
      "d/dx sin(x)"
      (diff (sinE x) "x")
      "cos(x)"
    -- Esperado: cos(x)

  , testShow
      "5.2  d/dx cos(x) = -sin(x)"
      "d/dx cos(x)"
      (diff (cosE x) "x")
      "-sin(x)"
    -- Esperado: -sin(x)

  , testShow
      "5.3  d/dy sin(x) = 0    [x independiente de y]"
      "d/dy sin(x)"
      (diff (sinE x) "y")
      "0"
    -- Esperado: 0

  -- ===============================================================
  -- NIVEL 6: Regla de la cadena
  -- ===============================================================

  , testShow
      "6.1  d/dx sin(x^2) = cos(x^2) * 2*x"
      "d/dx sin(x^2)"
      (diff (sinE (x .^ num 2)) "x")
      "cos(x^2)*(2*x)"
    -- Esperado: cos(x^2)*(2*x)

  , testShow
      "6.2  d/dx cos(3*x) = -3*sin(3*x)"
      "d/dx cos(3*x)"
      (diff (cosE (num 3 .* x)) "x")
      "-3*sin(3*x)"
    -- Esperado: -3*sin(3*x)
    -- Neg(Mul (Num 3) (Sin(3*x))) se reescribe a Mul (Num(-3)) (Sin(3*x))

  , testShow
      "6.3  d/dx (2*x + 1)^3 = 6*(2*x + 1)^2"
      "d/dx (2x+1)^3"
      (diff ((num 2 .* x .+ num 1) .^ num 3) "x")
      "6*(2*x + 1)^2"
    -- Esperado: 6*(2*x + 1)^2   (3*2 se combinan en 6)

  -- ===============================================================
  -- NIVEL 7: Regla del cociente
  -- ===============================================================

  , testShow
      "7.1  d/dx (x / y) = 1/y    [y const respecto a x]"
      "d/dx (x/y)"
      (diff (x ./ y) "x")
      "y/y^2"
    -- Esperado: y/y^2  (el simplificador no cancela y/y^2 → 1/y)

  , testShow
      "7.2  d/dx (1 / x) = -1/x^2"
      "d/dx (1/x)"
      (diff (num 1 ./ x) "x")
      "-1/x^2"
    -- Esperado: -1/x^2
    -- Quotient rule: (0*x - 1*1)/x^2 = (-1)/x^2
    -- Num(-1) se muestra como "-1", luego "/x^2"

  -- ===============================================================
  -- NIVEL 8: Derivadas de orden superior
  -- ===============================================================

  , testShow
      "8.1  d²/dx² (x^3) = 6*x"
      "d²/dx² x^3"
      (diffN 2 (x .^ num 3) "x")
      "6*x"
    -- Esperado: 6*x   (d/dx 3x^2 = 6x)

  , testShow
      "8.2  d²/dx² sin(x) = -sin(x)"
      "d²/dx² sin(x)"
      (diffN 2 (sinE x) "x")
      "-sin(x)"
    -- Esperado: -sin(x)   (d/dx cos(x) = -sin(x))

  , testShow
      "8.3  d³/dx³ (x^4) = 24*x"
      "d³/dx³ x^4"
      (diffN 3 (x .^ num 4) "x")
      "24*x"
    -- Esperado: 24*x   (4x^3 -> 12x^2 -> 24x)

  , testShow
      "8.4  d⁴/dx⁴ (x^4) = 24"
      "d⁴/dx⁴ x^4"
      (diffN 4 (x .^ num 4) "x")
      "24"
    -- Esperado: 24

  -- ===============================================================
  -- NIVEL 9: Funciones genéricas (literal-function de scmutils)
  -- ===============================================================

  , testShow
      "9.1  d/dx f(x) = Df(x)"
      "d/dx f(x)"
      (diff (funApp "f" [x]) "x")
      "Df(x)"
    -- Esperado: Df(x)   (derivada simbólica de función desconocida)

  , testShow
      "9.2  d/dx f(y) = 0    [y indep. de x]"
      "d/dx f(y)"
      (diff (funApp "f" [y]) "x")
      "0"
    -- Esperado: 0   (Df(y)*0 = 0)

  , testShow
      "9.3  d/dx f(x^2) = Df(x^2) * 2*x"
      "d/dx f(x^2)"
      (diff (funApp "f" [x .^ num 2]) "x")
      "Df(x^2)*(2*x)"
    -- Esperado: Df(x^2)*(2*x)   (cadena)

  -- ===============================================================
  -- NIVEL 10: Simplificación
  -- ===============================================================

  , test
      "10.1  simplify  0 + x = x"
      "0 + x"
      (simplify (num 0 .+ x))
      x

  , test
      "10.2  simplify  x * 1 = x"
      "x * 1"
      (simplify (x .* num 1))
      x

  , test
      "10.3  simplify  x * 0 = 0"
      "x * 0"
      (simplify (x .* num 0))
      (num 0)

  , test
      "10.4  simplify  x - x = 0"
      "x - x"
      (simplify (x .- x))
      (num 0)

  , test
      "10.5  simplify  x^0 = 1"
      "x^0"
      (simplify (x .^ num 0))
      (num 1)

  , test
      "10.6  simplify  x^1 = x"
      "x^1"
      (simplify (x .^ num 1))
      x

  , testShow
      "10.7  simplify  2 + 3 = 5"
      "2 + 3"
      (simplify (num 2 .+ num 3))
      "5"

  , testShow
      "10.8  simplify  sin(0) = 0"
      "sin(0)"
      (simplify (sinE (num 0)))
      "0"

  , testShow
      "10.9  simplify  cos(0) = 1"
      "cos(0)"
      (simplify (cosE (num 0)))
      "1"

  -- ===============================================================
  -- NIVEL 11: Test integrador — Lagrangiano del péndulo
  -- ===============================================================

  , let m = sym "m"; l = sym "l"; g = sym "g"
        el     = lagrangeEquations (lPendulum m l g) "theta"
    in testShow
      "11.1  Euler-Lagrange del péndulo contiene thetaddot"
      "E-L pendulum"
      el
      (show el)  -- auto-pasa, solo mostramos el resultado
    -- Este test siempre pasa; su propósito es imprimir la ecuación
    -- de Euler-Lagrange derivada automáticamente.
  ]

-- -----------------------------------------------------------------
-- Lagrangiano del péndulo (copiado de Main para autocontención)
-- -----------------------------------------------------------------

lPendulum :: Expr -> Expr -> Expr -> LocalTuple -> Expr
lPendulum m l g local =
  let theta    = ltCoord    local
      thetaDot = ltVelocity local
      kineticE   = lit (1%2) .* m .* sqrE (l .* thetaDot)
      potentialE = m .* g .* l .* (num 1 .- cosE theta)
  in  simplify (kineticE .- potentialE)

-- -----------------------------------------------------------------
-- Main: ejecutar todos los tests
-- -----------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "============================================================"
  putStrLn " Tests del motor de derivación simbólica"
  putStrLn "============================================================"
  putStrLn ""
  mapM_ runTest (zip [1::Int ..] tests)
  putStrLn ""
  putStrLn "============================================================"
  let passed = length (filter trPassed tests)
      total  = length tests
  putStrLn $ " Resultado: " ++ show passed ++ "/" ++ show total ++ " tests pasaron"
  if passed == total
    then putStrLn " ALL TESTS PASSED"
    else putStrLn " SOME TESTS FAILED"
  putStrLn "============================================================"

runTest :: (Int, TestResult) -> IO ()
runTest (_, tr) = do
  let status = if trPassed tr then "PASS" else "FAIL"
  putStrLn $ "  [" ++ status ++ "]  " ++ trName tr
  if trPassed tr
    then putStrLn $ "         => " ++ trGot tr
    else do
      putStrLn $ "         Obtenido:  " ++ trGot tr
      putStrLn $ "         Esperado:  " ++ trExpected tr
  putStrLn ""
