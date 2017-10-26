import System.IO
import System.Directory
import Data.Char
import Control.Monad
import Oraculo

pedirNuevaPrediccion :: IO Oraculo
pedirNuevaPrediccion = do
    putStrLn ""
    putStrLn "Escriba la predicción del nuevo oráculo:"
    prediccion <- getLine
    putStrLn ""
    let o = crearOraculo prediccion
    return o

persistir :: Oraculo -> IO ()
persistir o = do
    putStrLn ""
    putStrLn "Escriba el nombre del archivo:"
    archivo <- getLine
    putStrLn ""
    putStrLn "Escribiendo el archivo..."
    writeFile archivo . show $ o
    return ()

cargar :: IO Oraculo
cargar = do
    putStrLn ""
    putStrLn "Escriba el nombre del archivo:"
    archivo <- getLine
    putStrLn ""
    putStrLn "Abriendo el archivo..."
    fileExists <- doesFileExist archivo
    if fileExists
       then do 
            putStrLn "Leyendo el archivo..."
            contents <- readFile archivo
            let o = read contents :: Oraculo
            return o
        else do 
            putStrLn "El archivo no existe"
            putStrLn "¿Estas Seguro que escribiste bien el nombre?"
            putStrLn "Vuelve a intentar"
            putStrLn ""
            return . crearOraculo $ ""

predecir :: Oraculo -> IO Oraculo
predecir o@(Prediccion ps) = do
    putStrLn "Estoy pensando en......"
    putStrLn ps
    putStrLn "¿Estoy en lo correcto?"
    putStrLn "Si es así escribe si, de lo contrario escribe no."
    resp <- getLine
    case resp of
         "si" -> do
            putStrLn ""
            putStrLn "¡Sí! He triunfado."
            return o
         "no" -> do
            putStrLn "¡Vaya! Me he equivocado. "
            putStrLn "¿Cuál es la predicción correcta?"
            prediccion <- getLine
            let o' = Prediccion prediccion
            putStrLn ("¿Qué pregunta lo distingue de " ++ ps ++ "?")
            pregunta <- getLine
            putStrLn ("¿Cuál es la respuesta que lleva a " ++ prediccion ++ "?")
            opcion1 <- getLine
            putStrLn ("¿Cuál es la respuesta que lleva a " ++ ps ++ "?")
            opcion2 <- getLine
            let nuevoOra = ramificar [opcion2, opcion1] [o, o'] pregunta
            return nuevoOra
         otherwise -> do
            putStrLn "Escribiste una opción incorrecta, vuelve a intentarlo."
            return o
predecir o@(Pregunta qs os) = do
    putStrLn qs  
    mapM_ putStr (map (++" / ") (M.keys (opciones(o)))) 
    putStrLn " ninguna"
    resp <- getLine
    case resp of
        "ninguna" -> do
            putStrLn "¡Vaya!...¿Qué opción esperabas?"
            opcion <- getLine
            putStrLn "Ya que no sabía de la existencia de esa opción, ¿podrías decirme en qué pensaste?"
            ps <- getLine
            let predNueva = Prediccion ps 
            let listaOp = opcion:(M.keys(opciones (o)))
            let predicciones = predNueva:(M.elems(opciones (o)))
            let nuevoOra = ramificar (listaOp) (predicciones) qs 
            return nuevoOra
        
        otherwise -> do 
            if not (resp `elem` M.keys(opciones (o))) 
                then do 
                    putStrLn "Lo que introduciste no es una opción válida. Vuelve a intentar"
                    return o
            else do 
                nuevoOra <- predecir (respuesta(o) resp)
                let listaOp = M.insert resp nuevoOra os
                .0
                let oraculoNuevo = Pregunta qs listaOp
                return oraculoNuevo

pedirOpcion :: Oraculo -> IO ()
pedirOpcion o = do

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
            pedirOpcion o
         "2" -> do
            o' <- predecir o
            putStrLn ""
            pedirOpcion o'
         "3" -> do
            persistir o
            putStrLn "¡Se ha guardado el oráculo en el archivo correctamente!"
            putStrLn ""
            pedirOpcion o
         "4" -> do
            o <- cargar
            putStrLn "¡Se ha leido el oráculo en el archivo correctamente!"
            putStrLn ""
            pedirOpcion o
         "5" -> do
            putStrLn "Aqui se ejecuta funcion de opcion 5"
            pedirOpcion o
         "6" -> do
            putStrLn ""
            putStrLn "¡Gracias por jugar! Hasta Pronto."
            putStrLn ""
            return ()
         _ -> do
            putStrLn ""
            putStrLn "Ha escogido una opción no valida. Intente de nuevo."
            putStrLn ""
            pedirOpcion o


main = do
    putStrLn "¡Bienvenido a Haskinator!"
    putStrLn ""
    let o = crearOraculo ""
    pedirOpcion o
