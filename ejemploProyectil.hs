-- Módulo principal que ejecuta un ejemplo de cálculo de trayectoria de proyectil
module Main where

import qualified CalcularProyectil as CP

-- Función principal que simula el lanzamiento de un proyectil
main :: IO ()
main = do
  -- Definición de parámetros para la simulación del proyectil
  let { velocidadInicial = 10    -- velocidad inicial de lanzamiento (m/s)
      ; anguloRad = 0.5          -- ángulo de lanzamiento en radianes (~28.6 grados)
      ; tMax = 2                 -- tiempo máximo de simulación (segundos)
      ; deltaT = 0.1             -- incremento de tiempo entre cálculos (segundos)
      }

  -- Ejecuta el cálculo de la trayectoria del proyectil
  -- Imprimirá cada punto (x, y, t) hasta que el proyectil impacte el suelo (y <= 0)
  CP.trajectoria velocidadInicial anguloRad tMax deltaT
