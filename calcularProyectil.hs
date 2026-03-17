import Funciones (coseno, gradosARadianes, seno, miIterate, miMap, miTakeWhile)

-- Movimiento de proyectiles
-- x(t) = v0 cos(θ) t
-- y(t) = v0 sin(θ) t - (g t^2)/2
-- El cálculo se detiene al detectar impacto: primer t>0 con y(t) <= 0.

type Trayectoria = [(Float, Float, Float)] -- (t, x, y)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs) = x : if p x then [] else takeUntil p xs

trayectoriaProyectil :: Float -> Float -> Float -> Float -> Trayectoria
trayectoriaProyectil v0 thetaGrados dt g
  | dt <= 0 = error "dt debe ser > 0"
  | otherwise = takeUntil impacto (miMap estado tiempos)
  where
    theta = gradosARadianes thetaGrados
    vx = v0 * coseno theta
    vy = v0 * seno theta 0 0

    tiempos = miIterate (+ dt) 0

    estado t = (t, vx * t, vy * t - (g * t * t) / 2)

    impacto (t, _x, y) = t > 0 && y <= 0

-- Ejemplo del enunciado: retorna lista de parejas (x,y)
-- trajectory v0 thetaRad T dt
-- Se detiene al primer impacto (y <= 0) o cuando t > T.
trajectory :: Float -> Float -> Float -> Float -> [(Float, Float)]
trajectory v0 thetaRad tMax dt
  | dt <= 0 = error "dt debe ser > 0"
  | tMax < 0 = error "T debe ser >= 0"
  | otherwise = miMap toXY (takeUntil impacto (miMap estado tiempos))
  where
    g = 9.8
    vx = v0 * coseno thetaRad
    vy = v0 * seno thetaRad 0 0

    tiempos = miTakeWhile (<= tMax) (miIterate (+ dt) 0)
    estado t = (t, vx * t, vy * t - (g * t * t) / 2)
    toXY (_t, x, y) = (x, y)
    impacto (t, _x, y) = t > 0 && y <= 0

main :: IO ()
main = do
  let v0 = 10
      theta = gradosARadianes 28.64788975
      tMax = 2
      dt = 0.1

  print (trajectory v0 theta tMax dt)
  
