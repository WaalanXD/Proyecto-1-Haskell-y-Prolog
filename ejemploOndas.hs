-- Módulo principal que ejecuta ejemplos de cálculo de ondas
module Main where

import qualified CalcularOnda as CO

-- Función principal que presenta dos ejemplos de ondas
main :: IO ()
main = do
  -- EJEMPLO 1: Onda en modo TIEMPO
  -- Muestra cómo varía la amplitud en un punto fijo del espacio a lo largo del tiempo
  putStrLn "Ejemplo onda en modo TIEMPO (psi, t):"
  -- Parámetros:
  let kTiempo = 1        -- número de onda k
      wTiempo = 2        -- frecuencia angular ω
      xFijo = 2.5        -- posición fija en el espacio (metros)
      tMax = 2           -- tiempo máximo de simulación (segundos)
      dt = 0.1           -- incremento de tiempo entre cálculos
  -- Ejecuta el cálculo en modo "tiempo"
  CO.onda "tiempo" kTiempo wTiempo xFijo tMax dt

  -- Salto de línea para separar ejemplos
  putStrLn ""
  
  -- EJEMPLO 2: Onda en modo ESPACIO
  -- Muestra cómo varía la amplitud a lo largo del espacio en un tiempo fijo
  putStrLn "Ejemplo onda en modo ESPACIO (psi, x):"
  -- Parámetros:
  let kEspacio = 1       -- número de onda k
      wEspacio = 2       -- frecuencia angular ω
      tFijo = 0.5        -- tiempo fijo (segundos)
      lMax = 5           -- longitud máxima del espacio (metros)
      dx = 0.5           -- incremento de posición entre cálculos
  -- Ejecuta el cálculo en modo "espacio"
  CO.onda "espacio" kEspacio wEspacio tFijo lMax dx
