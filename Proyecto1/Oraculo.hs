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

-- Función que dado un String retorna un oráculo de tipo predicción con dicho String
-- como predicción
crearOraculo :: String -> Oraculo
crearOraculo ps = Prediccion ps


-- Función que dado un oráculo de tipo Predicción, retorna el String con su
-- predicción. Si recibe un oráculo de tipo Pregunta, ocasiona un error.
prediccion :: Oraculo -> String
prediccion (Prediccion ps) = ps
prediccion (Pregunta _ _) = error "¡Error! La función predicción debe recibir un oráculo de tipo Predicción."

-- Función que dado un oráculo de tipo Pregunta, retorna el String con la
-- pregunta asociada. Si recibe un oráculo de tipo Predicción, ocasiona un error.
pregunta :: Oraculo -> String
pregunta (Prediccion _) = error "¡Error! La función pregunta debe recibir un oráculo de tipo Pregunta."
pregunta (Pregunta ps _) = ps


-- Función que dado un oráculo de tipo Pregunta, retorna el String con las
-- opciones disponibles para esa pregunta. Si recibe un oráculo de tipo Predicción,
-- ocasiona un error.
opciones :: Oraculo -> Opciones
opciones (Prediccion _) = error "¡Error! La función opciones debe recibir un oráculo de tipo Pregunta."
opciones (Pregunta _ os) = os


-- Función que dado un oráculo de tipo Pregunta y un String, retorna un oráculo
-- que corresponde a la respuesta asociada al String. 
-- Si el String no corresponde a una opción válida del oráculo, devuelve un error. 
-- Si recibe un oráculo de tipo Predicción, ocasiona un error.
respuesta :: Oraculo -> String -> Oraculo
respuesta (Prediccion _) _ = error "¡Error! La función respuesta debe recibir un oráculo de tipo Pregunta."
respuesta (Pregunta _ os) rs = case Map.lookup rs os of
                                    Just o -> o
                                    Nothing -> error "¡Error! Esa respuesta no existe para la pregunta dada."


-- Función que dada una lista de Strings, una lista de oráculos y un String devuelve
-- un oráculo de tipo pregunta donde el String es la pregunta, La lista de Strings
-- son las opciones disponibles de dicha pregunta y la lista de oráculos las res-
-- puestas de las opciones
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar rs os xs = let ops = Map.fromList . zip rs $ os 
                     in Pregunta xs ops