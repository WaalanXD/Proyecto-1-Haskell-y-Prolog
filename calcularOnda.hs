-- Módulo para calcular y visualizar ondas sinusoidales
-- Simula comportamiento de ondas variando tiempo o espacio
module CalcularOnda where

import Funciones (aplicarATodos, ejecutarParaCadaUno_, iterar, seno, tomarMientras)

-- Función principal para calcular ondas
-- Parámetros: modo ("tiempo" o "espacio"), número de onda (k), frecuencia angular (w), 
--             posición fija x o tiempo fijo t, máximo de iteración, delta de incremento
onda :: String -> Float -> Float -> Float -> Float -> Float -> IO ()

-- CASO 1: MODO TIEMPO
-- x es fijo, t varía desde 0 hasta tMax
-- Simula cómo la amplitud varía en un punto fijo del espacio a lo largo del tiempo
onda "tiempo" k w x tMax dt = ejecutarParaCadaUno_ print resultados
  where
    -- Genera lista de tiempos: [0, dt, 2*dt, ..., hasta tMax]
    tiempos = tomarMientras (<= tMax) (iterar (+ dt) 0)
    -- Para cada tiempo, calcula la amplitud usando: sin(kx - wt)
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)
    -- Aplica el cálculo a todos los tiempos y retorna lista de (amplitud, tiempo)
    resultados = aplicarATodos calcularAmplitud tiempos

-- CASO 2: MODO ESPACIO
-- t es fijo, x varía desde 0 hasta lMax
-- Simula cómo la amplitud varía en un tiempo fijo a lo largo del espacio
onda "espacio" k w t lMax dx = ejecutarParaCadaUno_ print resultados
  where
    -- Genera lista de posiciones: [0, dx, 2*dx, ..., hasta lMax]
    posiciones = tomarMientras (<= lMax) (iterar (+ dx) 0)
    -- Para cada posición, calcula la amplitud usando: sin(kx - wt)
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)
    -- Aplica el cálculo a todas las posiciones y retorna lista de (amplitud, posición)
    resultados = aplicarATodos calcularAmplitud posiciones

-- CASO 3: MANEJO DE ERRORES
-- Si el usuario escribe un modo distinto a "tiempo" o "espacio", genera un error
onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"
