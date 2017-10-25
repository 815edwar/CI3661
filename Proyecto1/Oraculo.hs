module Oraculo 
( crearOraculo
, prediccion
, pregunta
, opciones
, respuesta
, ramificar
, Oraculo(..) 
) where

import qualified Data.Map as Map

type Opciones = Map.Map String Oraculo

data Oraculo = Prediccion String | Pregunta String Opciones deriving (Show, Read)

crearOraculo :: String -> Oraculo
crearOraculo ps = Prediccion ps

prediccion :: Oraculo -> String
prediccion (Prediccion ps) = ps
prediccion (Pregunta _ _) = error "Error! La funcion prediccion debe recibir un Oraculo de tipo Prediccion"

pregunta :: Oraculo -> String
pregunta (Prediccion _) = error "Error! La funcion pregunta debe recibir un Oraculo de tipo Pregunta"
pregunta (Pregunta ps _) = ps

opciones :: Oraculo -> Opciones
opciones (Prediccion _) = error "Error! La funcion opciones debe recibir un Oraculo de tipo Pregunta"
opciones (Pregunta _ os) = os

respuesta :: Oraculo -> String -> Oraculo
respuesta (Prediccion _) _ = error "Error! La funcion respuesta debe recibir un Oraculo de tipo Pregunta"
respuesta (Pregunta _ os) rs = case Map.lookup rs os of
                                    Just os -> os
                                    Nothing -> error "Error! Esa respuesta no existe para la pregunta dada"

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar rs os xs = let ops = Map.fromList . zip rs $ os 
                     in Pregunta xs ops