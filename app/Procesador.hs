module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, openFile, hClose, Handle, IOMode(..))
import System.Exit (exitWith, ExitCode(..))
import Alex (getToken)
import Token (Token(..), formatToken)
import GErrores           (GError, gErrorInicial, hayErrores, listarErrores)
import TablaSimbolos      (TablaSimbolos, tablaInicial, lexema)

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
    hTok    <- openFile "output/Tokens.txt"       WriteMode
    hTS     <- openFile "output/TablaSimbolos.txt" WriteMode

    --TODO: Esto ahora vale, pero cuando haya más tablas no se.
    --Se me ocurre escribir la tabla principal en este fichero, las demás
    --en otro fichero, cuando se acaba el programa, se imprimen ambas.
    --Así se mantiene el orden.
    -- Cabecera de la tabla de símbolos
    hPutStrLn hTS "CONTENIDO DE LA TABLA # 1 :"

    let ge0 = gErrorInicial
        ts0 = tablaInicial

    --Recorre todo el fichero
    geFinal <- bucle fichero ge0 ts0 hTok hTS

    hClose hTok
    hClose hTS

    if hayErrores geFinal
        then do
            hPutStrLn stderr "\n=== ERRORES ==="
            -- Función mapM_ para IO
            mapM_ (hPutStrLn stderr . show) (listarErrores geFinal)
            exitWith (ExitFailure 1)
        else putStrLn "\nSin errores léxicos."

-- Llama a getToken hasta TkEof, devuelve IO porque hace escritura(obligatorio)
bucle :: String -> GError -> TablaSimbolos -> Handle -> Handle -> IO GError
bucle fichero ge ts hTok hTS=
    case getToken fichero ge ts of
        (Just TkEof, _, ge', _, _) -> do
            --Imprimo el token en el fichero
            hPutStrLn hTok (formatToken TkEof)
            --Esto no es un return que conocemos, esto combierte a tipo IO
            return ge' 

        (Just tok, r, ge', ts', insertado) -> do
            --Imprimo el token en el fichero
            hPutStrLn hTok (formatToken tok)

            if insertado
                then case tok of
                    TkIdentificador pos -> do
                        --Si lexema devuelve Nothing, "" es el caso base
                        --A lo mejor innecesario porque si entro aquí debería haberlo metido
                        let iLex = maybe "" id (lexema pos ts')
                        hPutStrLn hTS ("* LEXEMA : '" ++ iLex ++ "'")
                        hPutStrLn hTS "        +Atributos:"

                    _ -> return () --TODO:Se supone es la acción vacía

                else return ()     --TODO:Se supone es la acción vacía

            bucle r ge' ts' hTok hTS

        --TODO:Puede haber insertado y ocurrir un error?
        (Nothing, r, ge', ts',_)    ->
            -- error léxico: ya quedó registrado en ge', seguimos
            bucle r ge' ts' hTok hTS
    
