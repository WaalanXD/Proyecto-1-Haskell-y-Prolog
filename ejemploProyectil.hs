
module Main where

import qualified CalcularProyectil as CP


main :: IO ()
main = do

  let { velocidadInicial = 10    
      ; anguloRad = 0.5          
      ; tMax = 2                 
      ; deltaT = 0.1             
      }


  CP.trajectoria velocidadInicial anguloRad tMax deltaT
