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

cargar :: IO (Maybe Oraculo)
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
            return (Just o)
        else do 
            putStrLn "El archivo no existe"
            putStrLn "¿Estas Seguro que escribiste bien el nombre?"
            putStrLn "Vuelve a intentar"
            putStrLn ""
            return Nothing

-- insertarPregunta :: Oraculo -> Oraculo -> String -> Oraculo
-- insertarPregunta pregunta (Prediccion p) ps
--                 | p == ps = pregunta
--                 | otherwise = Prediccion p

-- insertarPregunta pregunta () 

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
            putStrLn "¡Se ha inicializado el oráculo satisfactoriamente!"
            putStrLn ""
            pedirOpcion (Just o)
         "2" -> do
            putStrLn "Aqui se ejecuta funcion de opcion 2"
            pedirOpcion oraculo
         "3" -> do
            case oraculo of
                 Nothing -> do
                    putStrLn ""
                    putStrLn "Lo siento, no ha inicializado ningun oráculo."
                    putStrLn "Prueba crear uno nuevo con la opción 1 o cargar uno "
                    putStrLn "existente con la opcion 4"
                    putStrLn ""
                    pedirOpcion Nothing
                 Just o -> do
                    persistir o
                    putStrLn "¡Se ha guardado el oráculo en el archivo correctamente!"
                    putStrLn ""
                    pedirOpcion (Just o)
         "4" -> do
            oraculo <- cargar
            case oraculo of
                Nothing -> pedirOpcion oraculo
                Just _ -> do
                     putStrLn "¡Se ha leido el oráculo en el archivo correctamente"
                     putStrLn ""
                     pedirOpcion oraculo
         "5" -> do
            putStrLn "Aqui se ejecuta funcion de opcion 5"
            pedirOpcion oraculo
         "6" -> do
            putStrLn ""
            putStrLn "¡Gracias por jugar! Hasta Pronto."
            putStrLn ""
            return ()
         _ -> do
            putStrLn ""
            putStrLn "Ha escogido una opción no válida. Intente de nuevo."
            putStrLn ""
            pedirOpcion oraculo


main = do
    putStrLn "¡Bienvenido a Haskinator!"
    putStrLn ""
    pedirOpcion Nothing