module Asin (parsear) where

import Simbolos     (SimboloSem(..), NoTerminal(..), infoSemInicial, Simbolo (..), InfoSem(..))
import TablaLL      (tablaLL, Regla(..))
import Token       (Token(..), formatToken, mismoTipo)
import GErrores    (GError, registrarErrorAsin, CodErr (..), msgErrorNoTerminal, msgErrorTerminal)
import Alex        (getToken)
import TablaSimbolos (TablaSimbolos)
import System.IO   (Handle, hPutStr, hPutStrLn)
import Reglas       (numReglaInt)
import AccionesSem (aplicarAccion)

-- Pila inicial: [Axioma P, Dollar]
pilaInicial :: [SimboloSem]
pilaInicial = [axioma, eof]
            where axioma = SimboloSem{
                            simbolo = NoTerminal PP,
                            infoSem = infoSemInicial
                  }
                  eof = SimboloSem{
                            simbolo = Dollar,
                            infoSem = infoSemInicial
                  }

pilaAuxInicial :: [SimboloSem]
pilaAuxInicial = []

-- Punto de entrada del analizador sintáctico
-- Devuelve el GError tablaSimbolos actualizado y escribe: 
-- los tokens en hTok y las reglas en hParse
parsear :: String -> GError -> TablaSimbolos
        -> Handle          -- fichero de Tokens (Tokens.txt)
        -> Handle          -- fichero de reglas (Parse.txt)
        -> Handle          -- fichero de TS locales (Parse.txt)
        -> IO (GError, TablaSimbolos)
--
parsear input ge ts hTok hLocales hParse =
    let (tok, resto, ge', ts') = getToken input ge ts
    in case tok of
        --Error léxico, volvemos a probar
        Nothing -> parsear resto ge' ts' hTok hLocales hParse

        Just tok' -> do
            -- Escribir token en Tokens.txt
            hPutStrLn hTok (formatToken tok')

            bucleAsin resto ge' ts' hTok hLocales hParse pilaInicial pilaAuxInicial tok'

bucleAsin :: String -> GError -> TablaSimbolos 
            -> Handle -> Handle -> Handle
            -> [SimboloSem] -> [SimboloSem] -> Token
            -> IO (GError, TablaSimbolos)

-- Pila vacía: hemos terminado correctamente si el token es TkEof, si no, error
bucleAsin _ ge ts _ _ _ [] _ tok = do
    case tok of
        --Recordamos que return no devuelve, si no que lo convierte en monada IO
        TkEof -> return (ge,ts)
        _     -> return (registrarErrorAsin ErrTerminal
                    "El fichero contiene código tras el final esperado" ge, ts)

-- Cima es terminal
bucleAsin input ge ts hTok hLocales hParse (cima@(SimboloSem (Terminal tokenCima) _) : resto) aux tok =
    if mismoTipo tokenCima tok
        then do
            -- Consumimos el token, pedimos el siguiente
            let (tok', input', ge', ts') = getToken input ge ts
            case tok' of
                --Error léxico, borramos la pila y volvemos a empezar el analizador sintáctico
                Nothing -> parsear input' ge' ts' hTok hLocales hParse

                Just tokSig -> do
                    -- Escribir token en Tokens.txt
                    hPutStrLn hTok (formatToken tokSig)
                    -- En el caso terminal correcto, antes de meter cima en aux:
                    let cima' = case tok of
                                    TkIdentificador i -> cima { infoSem = (infoSem cima) { posTS = Just i } }
                                    _                 -> cima
                    bucleAsin input' ge' ts' hTok hLocales hParse resto (cima':aux) tokSig

        else do
            -- Error: terminal esperado distinto del token leído; reset pila
            let msg = "Se esperaba leer " ++ msgErrorTerminal tokenCima
                   ++ ", pero se leyó " ++ msgErrorTerminal tok
                ge' = registrarErrorAsin  ErrTerminal msg ge
            -- Borramos la pila y reseteamos el proceso
            parsear input ge' ts hTok hLocales hParse

-- Cima es no terminal
bucleAsin input ge ts hTok hLocales hParse (cima@(SimboloSem (NoTerminal nt) _) : resto) aux tok =
    case tablaLL nt tok of
        Nothing -> do
            -- Error: celda vacía; reset pila
            let msg = msgErrorNoTerminal nt
                ge' = registrarErrorAsin ErrNoTerminal msg ge

            -- Borramos la pila y reseteamos el proceso
            parsear input ge' ts hTok hLocales hParse

        Just (Regla n cons) -> do
            -- Escribimos el número de la regla
            hPutStr hParse (show (numReglaInt n) ++ " ")
            -- Añadimos el consecuente en orden inverso
            let pilaNueva = insertarCons cons resto
            bucleAsin input ge ts hTok hLocales hParse pilaNueva (cima:aux) tok

-- Cima es acción semántica
bucleAsin input ge ts hTok hLocales hParse ((SimboloSem (Accion acc) _) : resto) aux tok = do
    (ge', ts', resto', aux') <- aplicarAccion acc ge ts resto aux hLocales
    bucleAsin input ge' ts' hTok hLocales hParse resto' aux' tok

-- Dollar en medio (no debería ocurrir en uso normal)
bucleAsin _ ge ts _ _ _ ((SimboloSem Dollar _):_) _ _ = return (ge,ts)


insertarCons :: [Simbolo] -> [SimboloSem] -> [SimboloSem]
insertarCons cons pila = map (\s -> SimboloSem s infoSemInicial) cons ++ pila


