module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, openFile, hClose, IOMode(..))
import System.Exit (exitWith, ExitCode(..))
import GErrores           (gErrorInicial, hayErrores, listarErrores)
import TablaSimbolos      (tablaInicial)
import Asin (parsear)

-- Pasamos de un fichero a un string con el contenido del fichero
-- Esta función es lazy, luego convierte el fichero según sea necesario
main :: IO ()
main = do
    args <- getArgs
    fichero <- case args of
        [f] -> readFile f
        _ -> do
            hPutStrLn stderr "Uso: lexer <fichero>"
            exitWith (ExitFailure 1)

    -- Abrimos los ficheros de salida
    hTok    <- openFile "output/Tokens.txt"         WriteMode
    hTS     <- openFile "output/TablaSimbolos.txt"  WriteMode
    hParse  <- openFile "output/Parse.txt"          WriteMode

    --TODO: Esto ahora vale, pero cuando haya más tablas no se.
    --Se me ocurre escribir la tabla principal en este fichero, las demás
    --en otro fichero, cuando se acaba el programa, se imprimen ambas.
    --Así se mantiene el orden.
    -- Cabecera de la tabla de símbolos
    hPutStrLn hTS "CONTENIDO DE LA TABLA # 1 :"

    let ge0 = gErrorInicial
        ts0 = tablaInicial

    --Recorre todo el fichero
    geFinal <- parsear fichero ge0 ts0 hTok hTS hParse

    hClose hTok
    hClose hTS
    hClose hParse

    if hayErrores geFinal
        then do
            hPutStrLn stderr "\n=== ERRORES ==="
            -- Función mapM_ para IO
            mapM_ (hPutStrLn stderr . show) (listarErrores geFinal)
            exitWith (ExitFailure 1)
        else putStrLn "\nSin errores léxicos."

