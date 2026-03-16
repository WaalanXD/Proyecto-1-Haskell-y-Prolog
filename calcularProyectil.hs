module CalcularProyectil
  ( componentesVelocidad
  )
where

import Funciones (coseno, gradosARadianes, seno)

-- Ejemplo de uso de las funciones exportadas desde Funciones:
-- Dada una velocidad inicial y un ángulo en grados, retorna (vx, vy).
componentesVelocidad :: Float -> Float -> (Float, Float)
componentesVelocidad velocidad anguloGrados =
  let anguloRad = gradosARadianes anguloGrados
   in (velocidad * coseno anguloRad, velocidad * seno anguloRad)
