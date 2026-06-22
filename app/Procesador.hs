module Main (main) where

import System.Environment   (getArgs)
import System.IO            (hPutStrLn, stderr, openFile, hClose, IOMode(..))
import System.Exit          (exitWith, ExitCode(..))
import GErrores             (gErrorInicial, hayErrores, listarErrores)
import TablaSimbolos        (tablaInicial, formatearTabla, TablaSimbolos(tablaGlobal) )
import Asin                 (parsear)

-- Pasamos de un fichero a un string con el contenido del fichero
-- Esta función es lazy, luego convierte el fichero según sea necesario
-- El nombre del fichero a analizar se pasa como parámetro y se lee en args
main :: IO ()
main = do
    args <- getArgs
    fichero <- case args of
        [f] -> readFile f
        _ -> do
            hPutStrLn stderr "Uso: lexer <fichero>"
            exitWith (ExitFailure 1)

    -- Abrimos los ficheros de salida
    hTok        <- openFile "output/Tokens.txt"             WriteMode
    hTS         <- openFile "output/TablaSimbolos.txt"      WriteMode
    hParse      <- openFile "output/Parse.txt"              WriteMode
    hLocales    <- openFile "output/locales.txt"            WriteMode

    -- Cabecera de la tabla de símbolos global
    hPutStrLn hTS "CONTENIDO DE LA TABLA # 1 :"

    -- Inicializamos el gestor de errores y la tabla de símbolos
    let ge0 = gErrorInicial
        ts0 = tablaInicial

    -- Recorre todo el fichero y devuelve el gestor de errores y la tabla de símbolos resultante
    (geFinal, tsFinal) <- parsear fichero ge0 ts0 hTok hLocales hParse

    -- Escribimos la tabla global final en el fichero
    hPutStrLn hTS (formatearTabla (tablaGlobal tsFinal))

    --Cerramos los ficheros de salida
    hClose hTok
    hClose hTS
    hClose hParse
    hClose hLocales

    -- Era necesario primero cerrar los ficheros para poder abrirlos de esta manera
    -- Concatenamos las tablas locales del fichero auxiliar en el fichero de la tabla de símbolos
    contLocales <- readFile "output/locales.txt"
    appendFile "output/TablaSimbolos.txt" contLocales

    --Si hay errores, los imprimimos por la salida estandar
    if hayErrores geFinal
        then do
            hPutStrLn stderr "\n=== ERRORES ==="
            -- Función mapM_ para IO
            mapM_ (hPutStrLn stderr . show) (listarErrores geFinal)
            exitWith (ExitFailure 1)
        else putStrLn "\nSin errores léxicos."

