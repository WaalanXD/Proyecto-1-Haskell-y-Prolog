-- Módulo para calcular la trayectoria de un proyectil bajo gravedad
-- Utiliza ecuaciones de movimiento parabólico
module CalcularProyectil where

import Funciones (aplicarATodos, coseno, ejecutarParaCadaUno_, iterar, seno, tomarHasta, tomarMientras)

-- ECUACIONES DE MOVIMIENTO DE PROYECTILES:
-- x(t) = v0 * cos(θ) * t         (posición horizontal)
-- y(t) = v0 * sin(θ) * t - (g*t²)/2  (posición vertical)
-- g = 9.8 m/s² (aceleración de gravedad)
-- El cálculo se detiene cuando: primer t>0 donde y(t) <= 0 (impacto con el suelo)

-- Alias de tipo para representar trayectoria como lista de puntos (x, y, t)
type Trayectoria = [(Float, Float, Float)]

-- Calcula y visualiza la trayectoria de un proyectil
-- Parámetros:
--   velocidadInicial: velocidad inicial del proyectil (m/s)
--   anguloRad: ángulo de lanzamiento en radianes
--   tMax: tiempo máximo de simulación (segundos)
--   deltaT: incremento de tiempo entre cálculos (segundos)
-- Retorna: IO () después de imprimir todos los puntos de la trayectoria
-- Se detiene cuando: y <= 0 (impacto con el suelo) O t > tMax
trajectoria :: Float -> Float -> Float -> Float -> IO ()
trajectoria velocidadInicial anguloRad tMax deltaT
  -- Validaciones: deltaT debe ser positivo y tMax no puede ser negativo
  | deltaT <= 0 = error "deltaT debe ser > 0"
  | tMax < 0 = error "T debe ser >= 0"
  | otherwise = ejecutarParaCadaUno_ print resultados
  where
    -- Constante de gravedad terrestre
    gravedad = 9.8
    -- Componentes de velocidad inicial usando trigonometría
    velocidadX = velocidadInicial * coseno anguloRad -- componente horizontal (constante)
    velocidadY = velocidadInicial * seno anguloRad 0 0 -- componente vertical inicial

    -- Generar lista de tiempos: [0, deltaT, 2*deltaT, ..., hasta tMax]
    tiempos = tomarMientras (<= tMax) (iterar (+ deltaT) 0)
    -- Calcular posición en cada tiempo t: (t, x(t), y(t))
    estado t = (t, velocidadX * t, velocidadY * t - (gravedad * t * t) / 2)
    -- Reordenar a (x, y, t) para salida
    aXYT (t, x, y) = (x, y, t)
    -- Definir condición de impacto: tiene que ser t > 0 y altura y <= 0
    impacto (t, _x, y) = t > 0 && y <= 0
    -- Generar resultados: aplicar transformaciones y detener en el primer impacto
    resultados = aplicarATodos aXYT (tomarHasta impacto (aplicarATodos estado tiempos))
  
