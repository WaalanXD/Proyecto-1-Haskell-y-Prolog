-- onda modo k w fijo maximo delta
onda :: String -> Float -> Float -> Float -> Float -> Float -> [(Float, Float)]

-- 1. CASO TIEMPO: x es fijo, t varía hasta maximo (T)
onda "tiempo" k w x T dt = aplicarATodos calcularAmplitud tiempos
  where
    -- Generas la lista de tiempos igual que en el proyectil
    tiempos = tomarMientras (<= T) (iteraar (+ dt) 0)
    
    -- Aplicas la fórmula: sin(kx - wt). Asumimos A=1.
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)

-- 2. CASO ESPACIO: t es fijo, x varía hasta maximo (L)
onda "espacio" k w t L dx = AplicarATodos calcularAmplitud posiciones
  where
    -- Generas la lista de posiciones
    posiciones = tomarMientras (<= L) (iterar (+ dx) 0)
    
    -- Aplicas la misma fórmula
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)

-- 3. CASO DE ERROR: por si escriben mal el modo
onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"
