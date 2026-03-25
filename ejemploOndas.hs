module Main where

import qualified CalcularOnda as CO

main :: IO ()
main = do
  putStrLn "Ejemplo onda en modo TIEMPO (psi, t):"
  let kTiempo = 1
      wTiempo = 2
      xFijo = 2.5
      tMax = 2
      dt = 0.1
  CO.onda "tiempo" kTiempo wTiempo xFijo tMax dt

  putStrLn ""
  putStrLn "Ejemplo onda en modo ESPACIO (psi, x):"
  let kEspacio = 1
      wEspacio = 2
      tFijo = 0.5
      lMax = 5
      dx = 0.5
  CO.onda "espacio" kEspacio wEspacio tFijo lMax dx
