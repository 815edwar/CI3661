module Haskinator 
(main) where

import System.IO
import System.Directory
import Data.Char
import qualified Data.Map as M
import Control.Monad
import Data.Either
import Data.Char
import Oraculo

-- Función que pide al usuario un String y retorna un nuevo oráculo con el
-- String como nueva predicción.
pedirNuevaPrediccion :: IO Oraculo
pedirNuevaPrediccion = do
    putStrLn "\nEscriba la predicción del nuevo oráculo:"
    prediccion <- getLine
    putStrLn ""
    let o = crearOraculo prediccion
    return o


-- Función que recibe un oráculo, pide al usuario el nombre de un archivo 
-- y escribe en el el oráculo.
persistir :: Oraculo -> IO ()
persistir o = do
    putStrLn "\nEscriba el nombre del archivo:"
    archivo <- getLine
    putStrLn "\nEscribiendo el archivo..."
    writeFile archivo . show $ o
    return ()


-- Función que pide al usuario el nombre de un archivo. Si el archivo existe
-- crea un nuevo oráculo a partir de la información recibida del archivo de
-- texto. En caso contrario, devuelve Nothing
cargar :: IO (Maybe Oraculo)
cargar = do
    putStrLn "\nEscriba el nombre del archivo:"
    archivo <- getLine
    putStrLn "\nAbriendo el archivo..."
    fileExists <- doesFileExist archivo
    if fileExists
       then do 
            putStrLn "Leyendo el archivo..."
            contents <- readFile archivo
            let o = read contents :: Oraculo
            return (Just o)
        else do 
            putStrLn "El archivo no existe"
            putStrLn "¿Estas Seguro que escribiste bien el nombre?"
            putStrLn "Vuelve a intentar\n"
            return Nothing

-- Función que dado un oráculo, pide al usuario de dos String que sean
-- predicciones del oráculo. Luego le dice al usuario cual es la pregunta
-- que llevaría a decidir eventualmente por una prediccion u otra. Asi
-- como las opciones de respuesta correspondientes a cada una de las pre-
-- dicciones involucradas.
preguntaCrucial :: Oraculo -> IO ()
preguntaCrucial oraculo = do
    let instr = ["\nOh, ¡esta opción es interesante!",
                 "Necesitare las dos predicciones a las que quieres averiguar",
                 "su pregunta crucial.", 
                 "\nEscribe la primera prediccion: "]
    mapM_ putStrLn instr
    pred1 <- getLine
    putStrLn "\nEscribe la segunda prediccion: "
    pred2 <- getLine

    case buscarLCA pred1 pred2 oraculo of 
        Right list -> do
            let msj = ["\nLo siento. Esta consulta no es válida.",
                       "\nNo tengo conocimiento de que es ",
                       if list == [True, False] then pred2 else pred1,
                       "\nQuiza deberias seguir jugando para aprender mas sobre",
                       " las cosas que conoces. \n\n"]
            mapM_ putStr msj
            return ()
        Left datos -> do
            let msj = ["\nHmm... He encontrado la pregunta crucial de las",
                       " predicciones que consultaste.\n",
                       "\nLa pregunta crucial es: "]
            mapM_ putStr msj
            putStrLn (datos !! 0)
            let msj2 = ["La opción que lleva a ",
                        pred1,
                        " es: "]
            mapM_ putStr msj2 
            putStrLn (datos !! 1)
            let msj3 = ["La opción que lleva a ",
                        pred2,
                        " es: "]
            mapM_ putStr msj3 
            putStrLn (datos !! 2)
            putStrLn ""
            return ()

buscarLCA :: String -> String -> Oraculo -> Either [String] [Bool]
buscarLCA pred1 pred2 (Prediccion ps)
    | ps == pred1 = Right [True, False]
    | ps == pred2 = Right [False, True]
    | otherwise = Right [False, False]
buscarLCA pred1 pred2 oraculo = do
    let lefts = filter (\(_, elem) -> isLeft elem)
        rights = filter (\(_, elem) -> isRight elem)
        lefts' xs = map (\(op, elem) -> (op, fromLeft [] elem)) (lefts xs)
        rights' xs = map (\(op, elem) -> (op, fromRight [] elem)) (rights xs)
        partitionEithers xs = (lefts' xs, rights' xs)
        resp = map (\(op, o) -> (op, buscarLCA pred1 pred2 o)) (M.toList . opciones $ oraculo) 
        partition = partitionEithers resp

    case length . fst $ partition of
        0 -> do
            let trues = filter (\(_, elem) -> or elem) (snd partition)
            case length trues of
                0 -> Right [False, False]
                1 -> Right (snd . head $ trues)
                _ -> do
                    if (snd . head $ trues) == [True, False] 
                    then Left [pregunta oraculo, fst . head $ trues, fst . last $ trues]
                    else Left [pregunta oraculo, fst . last $ trues, fst . head $ trues]
        _ -> Left (snd . head . fst $ partition)


predecir :: Oraculo -> IO Oraculo
predecir o@(Prediccion ps) = do
    let msj = ["\nEstoy pensando en...",
               ps,
               "¿Estoy en lo correcto?",
               "\nSi es así escribe si, de lo contrario escribe no."]
    mapM_ putStrLn msj
    resp <- getLine

    case map toLower resp of
         "si" -> do
            putStrLn "\n¡Sí! He triunfado."
            putStrLn "Te enviare de nuevo al menú de opciones\n"
            return o
         "no" -> do
            putStrLn "\n¡Vaya! Me he equivocado."
            putStrLn "\n¿Cuál es la predicción correcta?"
            prediccion <- getLine
            let o' = Prediccion prediccion
            putStrLn ("\n¿Qué pregunta lo distingue de " ++ ps ++ "?")
            pregunta <- getLine
            putStrLn ("\n¿Cuál es la respuesta que lleva a " ++ prediccion ++ "?")
            opcion1 <- getLine
            putStrLn ("\n¿Cuál es la respuesta que lleva a " ++ ps ++ "?")
            opcion2 <- getLine
            let nuevoOra = ramificar [opcion2, opcion1] [o, o'] pregunta
            return nuevoOra
         _ -> do
            putStrLn "\nEscribiste una opción incorrecta, vuelve a intentarlo."
            return o
predecir o@(Pregunta qs os) = do
    putStrLn ""
    putStrLn qs  
    mapM_ putStr (map (++ " / ") (M.keys . opciones $ o)) 
    putStrLn " ninguna"
    resp <- getLine
    
    case map toLower resp of
        "ninguna" -> do
            putStrLn "\n¡Vaya!... ¿Qué opción esperabas?"
            opcion <- getLine
            putStrLn "Ya que no sabía de la existencia de esa opción, ¿podrías decirme en qué pensaste?"
            ps <- getLine
            let predNueva = Prediccion ps 
                listaOp = opcion:(M.keys(opciones (o)))
                predicciones = predNueva:(M.elems(opciones (o)))
                nuevoOra = ramificar (listaOp) (predicciones) qs 
            return nuevoOra
        _ -> do 
            if not (resp `elem` M.keys(opciones (o))) 
                then do 
                    putStrLn "\nLo que introduciste no es una opción válida. Vuelve a intentar"
                    return o
            else do 
                nuevoOra <- predecir (respuesta(o) resp)
                let listaOp = M.insert resp nuevoOra os
                    oraculoNuevo = Pregunta qs listaOp
                return oraculoNuevo


pedirOpcion :: Maybe Oraculo -> IO ()
pedirOpcion oraculo = do

    let opciones = ["######################## Menu ########################",
                    "# Escriba el número de la opción que desea ejecutar: #",
                    "# 1.- Crear un oráculo nuevo.                        #", 
                    "# 2.- Predecir.                                      #",
                    "# 3.- Persistir.                                     #", 
                    "# 4.- Cargar.                                        #", 
                    "# 5.- Consultar pregunta crucial.                    #",
                    "# 6.- Salir                                          #",
                    "######################################################",
                    "" ]

    mapM_ putStrLn opciones
    opcion <- getLine

    case opcion of
         "1" -> do
            o <- pedirNuevaPrediccion
            putStrLn "¡Se ha inicializado el oráculo satisfactoriamente!\n"
            pedirOpcion (Just o)
         "2" -> do
            case oraculo of
                Nothing -> do
                    let error = ["",
                                 "Lo siento, no ha inicializado ningun oráculo.",
                                 "Prueba crear uno nuevo con la opción 1 o cargar uno ",
                                 "existente con la opcion 4",
                                 ""]
                    mapM_ putStrLn error
                    pedirOpcion Nothing
                Just o -> do
                    o' <- predecir o
                    putStrLn ""
                    pedirOpcion (Just o')
         "3" -> do
            case oraculo of
                 Nothing -> do
                    let error = ["",
                                 "Lo siento, no ha inicializado ningun oráculo.",
                                 "Prueba crear uno nuevo con la opción 1 o cargar uno ",
                                 "existente con la opcion 4",
                                 ""]
                    mapM_ putStrLn error
                    pedirOpcion Nothing
                 Just o -> do
                    persistir o
                    putStrLn "¡Se ha guardado el oráculo en el archivo correctamente!\n"
                    pedirOpcion (Just o)
         "4" -> do
            oraculo <- cargar
            case oraculo of
                Nothing -> pedirOpcion oraculo
                Just _ -> do
                     putStrLn "¡Se ha leido el oráculo en el archivo correctamente\n"
                     pedirOpcion oraculo
         "5" -> do
            case oraculo of
                Nothing -> do
                    let error = ["",
                                 "Lo siento, no ha inicializado ningun oráculo.",
                                 "Prueba crear uno nuevo con la opción 1 o cargar uno ",
                                 "existente con la opcion 4",
                                 ""]
                    mapM_ putStrLn error
                    pedirOpcion Nothing
                Just o -> do
                    preguntaCrucial o
                    pedirOpcion oraculo
         "6" -> do
            putStrLn "\n¡Gracias por jugar! Hasta Pronto.\n"
            return ()
         _ -> do
            putStrLn "\nHa escogido una opción no válida. Intente de nuevo.\n"
            pedirOpcion oraculo


main = do
    putStrLn "¡Bienvenido a Haskinator!\n"
    pedirOpcion Nothing
