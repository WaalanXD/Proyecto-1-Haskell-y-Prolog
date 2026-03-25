module Main where

import Funciones (aplicarATodos, coseno, gradosARadianes, iterar, seno, tomarHasta, tomarMientras, mapearM_)

-- Movimiento de proyectiles
-- x(t) = v0 cos(θ) t
-- y(t) = v0 sin(θ) t - (g t^2)/2
-- El cálculo se detiene al detectar impacto: primer t>0 con y(t) <= 0.

type Trayectoria = [(Float, Float, Float)] -- (x, y, t)

-- Retorna una lista de triples (x, y, t).
-- Se detiene al primer impacto (y <= 0) o cuando t > T.
trajectory :: Float -> Float -> Float -> Float -> Trayectoria
trajectory velocidadInicial anguloRad tMax deltaT
  | deltaT <= 0 = error "deltaT debe ser > 0"
  | tMax < 0 = error "T debe ser >= 0"
  | otherwise = aplicarATodos aXYT (tomarHasta impacto (aplicarATodos estado tiempos))
  where
    gravedad = 9.8
    velocidadX = velocidadInicial * coseno anguloRad
    velocidadY = velocidadInicial * seno anguloRad 0 0

    tiempos = tomarMientras (<= tMax) (iterar (+ deltaT) 0)
    estado t = (t, velocidadX * t, velocidadY * t - (gravedad * t * t) / 2)
    aXYT (t, x, y) = (x, y, t)
    impacto (t, _x, y) = t > 0 && y <= 0

main :: IO ()
main = do
  let { velocidadInicial = 10
      ; anguloRad = 0.5
      ; tMax = 2
      ; deltaT = 0.1
      }


  mapearM_ print (trajectory velocidadInicial anguloRad tMax deltaT)
  
