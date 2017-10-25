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

-- insertarPregunta 

-- predecir :: Oraculo -> IO Oraculo
-- predecir (Prediccion ps) = do
--     putStrLn "Estoy pensando en......"
--     putStrLn ps
--     putStrLn "¿Estoy en lo correcto?"
--     putStrLn "Si es así escribe si, de lo contrario escribe no."
--     resp <- getLine
--     case toLower resp of
--          "si" -> return o
--          "no" -> do
--             putStrLn "¡Vaya! Me he equivocado. "
--             putStrLn "¿Cuál es la predicción correcta?"
--             prediccion <- getLine
--             putStrLn ("¿Qué pregunta lo distingue de " ++ ps)
--             pregunta <- getLine
--             putStrLn ("¿Cuál es la respuesta que lleva a " ++ prediccion)
--             opcion1 <- getLine
--             putStrLn ("¿Cuál es la respuesta que lleva a " ++ ps)
--             opcion2 <- getLine
--             let qs = ramificar [opcion1, opcion2] 




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
            putStrLn "Aqui se ejecuta funcion de opcion 2"
            pedirOpcion o
         "3" -> do
            persistir o
            putStrLn "¡Se ha guardado el oráculo en el archivo correctamente!"
            putStrLn ""
            pedirOpcion o
         "4" -> do
            o <- cargar
            print o
            putStrLn "¡Se ha leido el oráculo en el archivo correctamente"
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