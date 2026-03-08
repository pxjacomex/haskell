# Mecánica simbólica — Lagrangianos y Hamiltonianos

Port del demo de [scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt) (MIT Scheme) a Haskell puro.

Deriva automáticamente ecuaciones de Euler-Lagrange y Hamiltonianos
a partir de Lagrangianos simbólicos, usando un motor de álgebra simbólica autocontenido.

## Requisitos

- **GHC** >= 8.10 (Haskell compiler)
- **cabal-install** >= 3.0

No se requieren bibliotecas externas; el proyecto solo depende de `base`.

### Instalar GHC y cabal

**Ubuntu / Debian:**

```bash
sudo apt update
sudo apt install ghc cabal-install
```

**Con [GHCup](https://www.haskell.org/ghcup/) (recomendado, cualquier plataforma):**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

GHCup instala `ghc` y `cabal` en `~/.ghcup/bin`.

**macOS (Homebrew):**

```bash
brew install ghc cabal-install
```

**Arch Linux:**

```bash
sudo pacman -S ghc cabal-install
```

## Compilación

```bash
cabal update
cabal build all
```

## Ejecución

### Demo principal (péndulo)

```bash
cabal run pendulum-demo
```

Salida esperada:

```
=====================================================
 Péndulo simple — Ecuaciones de Euler-Lagrange
 (port de scmutils a Haskell)
=====================================================

Lagrangiano  L(t, θ, θ̇):
  ...

Ecuación de Euler-Lagrange  (= 0):
  ...

Es decir:
  m*l²*θ̈ + m*g*l*sin(θ) = 0

(Dividiendo por m*l se obtiene la ecuación clásica:
   l*θ̈ + g*sin(θ) = 0 )
```

### Demo péndulo doble (Lagrangiano → Hamiltoniano)

```bash
cabal run double-pendulum-demo
```

Modela un péndulo doble con **5 parámetros simbólicos** (`m1`, `m2`, `l1`, `l2`, `g`)
y 2 coordenadas generalizadas (`θ₁`, `θ₂`).

El programa calcula y muestra:

1. El **Lagrangiano** L = T − V
2. Los **momentos conjugados** p₁ = ∂L/∂θ̇₁, p₂ = ∂L/∂θ̇₂
3. El **Hamiltoniano** H = Σ θ̇ᵢ·pᵢ − L (transformada de Legendre)

Salida esperada (estructura):

```
=====================================================
 Péndulo doble — Lagrangiano y Hamiltoniano
 5 parámetros simbólicos: m1, m2, l1, l2, g
=====================================================

Lagrangiano  L(θ₁, θ₂, θ̇₁, θ̇₂):
  ...

Momentos conjugados:
  p_theta1 = ∂L/∂theta1dot =
    ...
  p_theta2 = ∂L/∂theta2dot =
    ...

Hamiltoniano  H = Σ θ̇ᵢ·pᵢ − L  (en coordenadas y velocidades):
  ...
```

### Tests del motor simbólico

```bash
cabal run test-symbolic
```

Ejecuta 30 tests progresivos que verifican el motor de derivación simbólica:

| Nivel | Qué se prueba | Tests |
|-------|---------------|-------|
| 1 | Constantes y variables | `d/dx(5)=0`, `d/dx(x)=1`, `d/dx(y)=0` |
| 2 | Linealidad (suma, resta, negación) | `d/dx(x+y)=1`, `d/dx(-x)=-1`, etc. |
| 3 | Regla del producto | `d/dx(x*y)=y`, `d/dx(x*x)=x+x` |
| 4 | Regla de la potencia | `d/dx(x²)=2*x`, `d/dx(x³)=3*x²` |
| 5 | Funciones trigonométricas | `d/dx sin(x)=cos(x)`, `d/dx cos(x)=-sin(x)` |
| 6 | Regla de la cadena | `d/dx sin(x²)=cos(x²)*(2*x)` |
| 7 | Regla del cociente | `d/dx(1/x)=-1/x²` |
| 8 | Derivadas de orden superior | `d²/dx²(x³)=6*x`, `d⁴/dx⁴(x⁴)=24` |
| 9 | Funciones genéricas (`literal-function`) | `d/dx f(x)=Df(x)` |
| 10 | Simplificación algebraica | `0+x=x`, `x*0=0`, `sin(0)=0` |
| 11 | Test integrador: Euler-Lagrange del péndulo | Ecuación completa |

Salida esperada (resumen):

```
 Resultado: 30/30 tests pasaron
 ALL TESTS PASSED
```

## Estructura del proyecto

```
pendulum-lagrangian.cabal          -- Definición del proyecto
src/
  Symbolic/
    Expr.hs                        -- Tipo Expr (árbol de expresiones simbólicas)
    Simplify.hs                    -- Simplificador algebraico por reescritura
    Differentiation.hs             -- Diferenciación simbólica (cadena, producto, etc.)
  Mechanics/
    Lagrangian.hs                  -- Euler-Lagrange automático
    Hamiltonian.hs                 -- Transformada de Legendre (L → H)
app/
  Main.hs                         -- Demo del péndulo simple
  DoublePendulum.hs               -- Demo del péndulo doble (Lagrangiano + Hamiltoniano)
  TestSymbolic.hs                  -- Suite de tests
```

## Correspondencia con scmutils

| scmutils (Scheme) | Haskell |
|--------------------|---------|
| `'m`, `'l`, `'g` | `sym "m"`, `sym "l"`, `sym "g"` |
| `(coordinate local)` | `ltCoord local` |
| `(velocity local)` | `ltVelocity local` |
| `(literal-function 'theta)` | `funApp "theta" [sym "t"]` |
| `(Lagrange-equations L)` | `lagrangeEquations L "theta"` |
| `show-expression` | `showExpression` |
| — | `conjugateMomenta` |
| — | `legendreTransform` |
