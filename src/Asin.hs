module Asin (parsear) where

import Simbolos     (SimboloSem(..), NoTerminal(..), infoSemInicial, msgErrorNoTerminal, msgErrorTerminal, Simbolo (..), InfoSem(..))
import TablaLL      (tablaLL, Regla(..))
import Token       (Token(..), formatToken, mismoTipo)
import GErrores    (GError, registrarErrorAsin, CodErr (..))
import Alex        (getToken)
import TablaSimbolos (TablaSimbolos, lexema)
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


-- Función para escribir en la tablaSimbolos
insertadoEnTs :: Token -> Bool -> TablaSimbolos -> Handle -> IO()
insertadoEnTs (TkIdentificador pos) True ts hTS = do
    let iLex = maybe "" id (lexema pos ts)
    hPutStrLn hTS ("* LEXEMA : '" ++ iLex ++ "'")
    hPutStrLn hTS "        +Atributos:"
    --HPutStrLn ya es un IO, luego no hay que hacer el return

-- El return es porque es una función que escribe en un fichero, luego necesita
-- devolver un IO, devolvemos el vacio siempre
-- Este caso es por si no queremos escribir en la tabla
insertadoEnTs _ _ _ _ = return () 


-- Punto de entrada del analizador sintáctico
-- Devuelve el GError actualizado y escribe: 
-- los tokens en hTok, la tablaSimbolos hTS y las reglas en hParse
parsear :: String -> GError -> TablaSimbolos
        -> Handle          -- fichero de Tokens (Tokens.txt)
        -> Handle          -- fichero de TablaSimbolos (TablaSimbolos.txt)
        -> Handle          -- fichero de reglas (Parse.txt)
        -> IO GError
--
parsear input ge ts hTok hTS hParse =
    let (tok, resto, ge', ts', insertado) = getToken input ge ts
    in case tok of
        --Error léxico, volvemos a probar
        Nothing -> parsear resto ge' ts' hTok hTS hParse

        Just tok' -> do
            -- Escribir token en Tokens.txt
            hPutStrLn hTok (formatToken tok')
            -- Si es identificador nuevo, escribir en TablaSimbolos.txt
            insertadoEnTs tok' insertado ts' hTS

            bucleAsin resto ge' ts' hTok hTS hParse pilaInicial pilaAuxInicial tok'

bucleAsin :: String -> GError -> TablaSimbolos 
            -> Handle -> Handle -> Handle
            -> [SimboloSem] -> [SimboloSem] -> Token
            -> IO GError

-- Pila vacía: hemos terminado correctamente si el token es TkEof, si no, error
bucleAsin _ ge _ _ _ _ [] _ tok = do
    case tok of
        --Recordamos que return no devuelve, si no que lo convierte en monada IO
        TkEof -> return ge
        _     -> return (registrarErrorAsin ErrTerminal
                    "El fichero contiene código tras el final esperado" ge)

-- Cima es terminal
-- También se puede hacer con una función por partes
bucleAsin input ge ts hTok hTS hParse (cima@(SimboloSem (Terminal tokenCima) _) : resto) aux tok =
    if mismoTipo tokenCima tok
        then do
            -- Consumimos el token, pedimos el siguiente
            let (tok', input', ge', ts', insertado) = getToken input ge ts
            case tok' of
                --Error léxico, borramos la pila y volvemos a empezar el analizador sintáctico
                Nothing -> parsear input' ge' ts' hTok hTS hParse

                Just tokSig -> do
                    -- Escribir token en Tokens.txt
                    hPutStrLn hTok (formatToken tokSig)
                    -- Si es identificador nuevo, escribir en TablaSimbolos.txt
                    insertadoEnTs tokSig insertado ts' hTS
                    -- En el caso terminal correcto, antes de meter cima en aux:
                    let cima' = case tok of
                                    TkIdentificador i -> cima { infoSem = (infoSem cima) { posTS = Just i } }
                                    _                 -> cima
                    bucleAsin input' ge' ts' hTok hTS hParse resto (cima':aux) tokSig

        else do
            -- Error: terminal esperado distinto del token leído; reset pila
            let msg = "Se esperaba leer " ++ msgErrorTerminal tokenCima
                   ++ ", pero se leyó " ++ msgErrorTerminal tok
                ge' = registrarErrorAsin  ErrTerminal msg ge
            -- Borramos la pila y reseteamos el proceso
            parsear input ge' ts hTok hTS hParse

-- Cima es no terminal
bucleAsin input ge ts hTok hTS hParse (cima@(SimboloSem (NoTerminal nt) _) : resto) aux tok =
    case tablaLL nt tok of
        Nothing -> do
            -- Error: celda vacía; reset pila
            let msg = msgErrorNoTerminal nt
                ge' = registrarErrorAsin ErrNoTerminal msg ge

            -- Borramos la pila y reseteamos el proceso
            parsear input ge' ts hTok hTS hParse

        Just (Regla n cons) -> do
            -- Escribimos el número de la regla
            hPutStr hParse (show (numReglaInt n) ++ " ")
            -- Añadimos el consecuente en orden inverso
            let pilaNueva = insertarCons cons resto
            bucleAsin input ge ts hTok hTS hParse pilaNueva (cima:aux) tok

-- Cima es acción semántica
bucleAsin input ge ts hTok hTS hParse ((SimboloSem (Accion acc) _) : resto) aux tok = do
    (ge', ts', resto', aux') <- aplicarAccion acc ge ts resto aux
    bucleAsin input ge' ts' hTok hTS hParse resto' aux' tok

-- Dollar en medio (no debería ocurrir en uso normal)
bucleAsin _ ge _ _ _ _ ((SimboloSem Dollar _):_) _ _ = return ge


insertarCons :: [Simbolo] -> [SimboloSem] -> [SimboloSem]
insertarCons cons pila = map (\s -> SimboloSem s infoSemInicial) cons ++ pila


