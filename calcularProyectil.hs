module CalcularProyectil where

import Funciones (aplicarATodos, coseno, ejecutarParaCadaUno_, iterar, seno, tomarHasta, tomarMientras)



type Trayectoria = [(Float, Float, Float)] 

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
  
