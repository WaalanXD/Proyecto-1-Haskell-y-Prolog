module CalcularProyectil where

import Funciones (aplicarATodos, coseno, ejecutarParaCadaUno_, iterar, seno, tomarHasta, tomarMientras)

-- Movimiento de proyectiles
-- x(t) = v0 cos(θ) t
-- y(t) = v0 sin(θ) t - (g t^2)/2
-- El cálculo se detiene al detectar impacto: primer t>0 con y(t) <= 0.

type Trayectoria = [(Float, Float, Float)] -- (x, y, t)

-- Retorna IO () después de procesar todos los puntos de la trayectoria.
-- Se detiene al primer impacto (y <= 0) o cuando t > T.
trajectoria :: Float -> Float -> Float -> Float -> IO ()
trajectoria velocidadInicial anguloRad tMax deltaT
  | deltaT <= 0 = error "deltaT debe ser > 0"
  | tMax < 0 = error "T debe ser >= 0"
  | otherwise = ejecutarParaCadaUno_ print resultados
  where
    gravedad = 9.8
    velocidadX = velocidadInicial * coseno anguloRad
    velocidadY = velocidadInicial * seno anguloRad 0 0

    tiempos = tomarMientras (<= tMax) (iterar (+ deltaT) 0)
    estado t = (t, velocidadX * t, velocidadY * t - (gravedad * t * t) / 2)
    aXYT (t, x, y) = (x, y, t)
    impacto (t, _x, y) = t > 0 && y <= 0
    resultados = aplicarATodos aXYT (tomarHasta impacto (aplicarATodos estado tiempos))
  
