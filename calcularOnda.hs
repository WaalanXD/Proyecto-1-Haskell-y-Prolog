module Main where

import Funciones (aplicarATodos, iterar, ejecutarParaCadaUno_, seno, tomarMientras)

-- onda modo k w fijo maximo delta
onda :: String -> Float -> Float -> Float -> Float -> Float -> [(Float, Float)]

-- 1. CASO TIEMPO: x es fijo, t varía hasta maximo (T)


onda "tiempo" k w x tMax dt = aplicarATodos calcularAmplitud tiempos
  where
    -- Generas la lista de tiempos igual que en el proyectil
    tiempos = tomarMientras (<= tMax) (iterar (+ dt) 0)
    
    -- Aplicas la fórmula: sin(kx - wt). Asumimos A=1.
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)

-- 2. CASO ESPACIO: t es fijo, x varía hasta maximo (L)

onda "espacio" k w t lMax dx = aplicarATodos calcularAmplitud posiciones
  where
    -- Generas la lista de posiciones
    posiciones = tomarMientras (<= lMax) (iterar (+ dx) 0)
    
    -- Aplicas la misma fórmula
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)



-- 3. CASO DE ERROR: por si escriben mal el modo
onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"


--EJEMPLO:
-- En GHCi puedes llamar:
--   onda "tiempo" 1 2 2.5 2 0.1
--   onda "espacio" 1 2 0.5 5 0.5
-- y te retorna una lista de tuplas (psi, t).

main :: IO ()
main = do
  putStrLn "Ejemplo onda en modo TIEMPO (psi, t):"
  let kTiempo = 1
      wTiempo = 2
      xFijo = 2.5
      tMax = 2
      dt = 0.1
  ejecutarParaCadaUno_ print (onda "tiempo" kTiempo wTiempo xFijo tMax dt)

  putStrLn ""
  putStrLn "Ejemplo onda en modo ESPACIO (psi, x):"
  let kEspacio = 1
      wEspacio = 2
      tFijo = 0.5
      lMax = 5
      dx = 0.5
  ejecutarParaCadaUno_ print (onda "espacio" kEspacio wEspacio tFijo lMax dx)
