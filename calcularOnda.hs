
-- Módulo para calcular y visualizar ondas sinusoidales
-- Soporta dos modos: evolución temporal en posición fija o perfil espacial en tiempo fijo
module CalcularOnda where

import Funciones (aplicarATodos, ejecutarParaCadaUno_, iterar, seno, tomarMientras)

-- ECUACIONES DE ONDAS:
-- ψ(x,t) = A * sin(kx - ωt)   (onda viajera unidimensional)
-- k = número de onda
-- ω = frecuencia angular
-- El cálculo muestra pares (amplitud, parámetro_variable)

-- Calcula y visualiza una onda sinusoidal en dos modos diferentes
-- Modo "tiempo": muestra evolución temporal en posición fija
-- Modo "espacio": muestra perfil espacial en tiempo fijo
-- Parámetros:
--   modo: "tiempo" o "espacio"
--   k: número de onda (determina longitud de onda: λ = 2π/k)
--   w: frecuencia angular (determina período: T = 2π/ω)
--   x: posición fija (usado en modo "tiempo")
--   t: tiempo fijo (usado en modo "espacio")
--   tMax/lMax: límite máximo del parámetro variable (segundos o metros)
--   dt/dx: incremento entre cálculos (resolución de la simulación)
-- Retorna: IO () después de imprimir todas las amplitudes calculadas
onda :: String -> Float -> Float -> Float -> Float -> Float -> IO ()

-- CASO 1: Modo TIEMPO - Observar amplitud en un punto del espacio a lo largo del tiempo
-- Parámetros: "tiempo" k w posicionFija tMax deltaT
-- Utilidad: ver cómo oscila la onda en una ubicación específica
onda "tiempo" k w x tMax dt = ejecutarParaCadaUno_ print resultados
  where
    -- Generar lista de tiempos: [0, dt, 2*dt, ..., hasta tMax]
    -- tomarMientras detiene cuando la condición (<= tMax) se hace falsa
    tiempos = tomarMientras (<= tMax) (iterar (+ dt) 0)

    -- Función que calcula la amplitud en el punto fijo x en cada instante t
    -- Usa la ecuación: ψ = sin(kx - ωt)
    -- Retorna tupla: (amplitud, tiempo)
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)

    -- Aplicar la función calcularAmplitud a todos los tiempos generados
    -- Produce lista de tuplas [(ψ(x,0), 0), (ψ(x,dt), dt), ..., (ψ(x,tMax), tMax)]
    resultados = aplicarATodos calcularAmplitud tiempos


-- CASO 2: Modo ESPACIO - Observar amplitud a lo largo del espacio en un instante
-- Parámetros: "espacio" k w tiempoFijo lMax deltaX
-- Utilidad: ver la forma de la onda congelada en el tiempo (fotografía de la onda)
onda "espacio" k w t lMax dx = ejecutarParaCadaUno_ print resultados
  where
    -- Generar lista de posiciones: [0, dx, 2*dx, ..., hasta lMax]
    -- tomarMientras detiene cuando la condición (<= lMax) se hace falsa
    posiciones = tomarMientras (<= lMax) (iterar (+ dx) 0)

    -- Función que calcula la amplitud a lo largo del espacio en el instante t fijo
    -- Usa la ecuación: ψ = sin(kx - ωt)
    -- Retorna tupla: (amplitud, posicion)
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)
   
    -- Aplicar la función calcularAmplitud a todas las posiciones generadas
    -- Produce lista de tuplas [(ψ(0,t), 0), (ψ(dx,t), dx), ..., (ψ(lMax,t), lMax)]
    resultados = aplicarATodos calcularAmplitud posiciones


-- CASO 3: Error - Modo no reconocido
-- Si el usuario proporciona un modo diferente a "tiempo" o "espacio", lanza error
onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"
