import Funciones (aplicarATodos, coseno, gradosARadianes, iterar, seno, tomarHasta, tomarMientras)

-- Movimiento de proyectiles
-- x(t) = v0 cos(θ) t
-- y(t) = v0 sin(θ) t - (g t^2)/2
-- El cálculo se detiene al detectar impacto: primer t>0 con y(t) <= 0.

type Trayectoria = [(Float, Float, Float)] -- (t, x, y)

-- Ejemplo del enunciado: retorna lista de parejas (x,y)
-- Se detiene al primer impacto (y <= 0) o cuando t > T.
trayectoriaXY :: Float -> Float -> Float -> Float -> [(Float, Float)]
trayectoriaXY velocidadInicial anguloRad tMax deltaT
  | deltaT <= 0 = error "deltaT debe ser > 0"
  | tMax < 0 = error "T debe ser >= 0"
  | otherwise = aplicarATodos aXY (tomarHasta impacto (aplicarATodos estado tiempos))
  where
    gravedad = 9.8
    velocidadX = velocidadInicial * coseno anguloRad
    velocidadY = velocidadInicial * seno anguloRad 0 0

    tiempos = tomarMientras (<= tMax) (iterar (+ deltaT) 0)
    estado t = (t, velocidadX * t, velocidadY * t - (gravedad * t * t) / 2)
    aXY (_t, x, y) = (x, y)
    impacto (t, _x, y) = t > 0 && y <= 0

main :: IO ()
main = do
  let velocidadInicial = 10
      anguloRad = gradosARadianes 28.64788975
      tMax = 2
      deltaT = 0.1

  print (trayectoriaXY velocidadInicial anguloRad tMax deltaT)
  
