module TablaSimbolos where

import Simbolos (TipoSem(..), InfoSem(..), infoSemInicial)

data FilaTS = FilaTS
    { lexemaTS :: String
    , infoTS   :: InfoSem
    } deriving (Show)

newtype Tabla = Tabla [FilaTS] deriving (Show)

data TablaSimbolos = TablaSimbolos
    { tablaGlobal :: Tabla
    , tablaLocal  :: Maybe Tabla   -- Nothing si no hay función activa
    } deriving (Show)

tablaInicial :: TablaSimbolos
tablaInicial = TablaSimbolos
    { tablaGlobal = Tabla []
    , tablaLocal  = Nothing
    }

-- Crea una tabla local vacía al entrar en una función
crearTablaLocal :: TablaSimbolos -> TablaSimbolos
crearTablaLocal ts = ts { tablaLocal = Just (Tabla []) }

-- Destruye la tabla local al salir de la función
liberarTablaLocal :: TablaSimbolos -> TablaSimbolos
liberarTablaLocal ts = ts { tablaLocal = Nothing }

-- Devuelve (posición, tabla actualizada, fue insertado)
-- Posiciones positivas: tabla global
-- Posiciones negativas: tabla local (convenio del enunciado)
insertarOBuscar :: String -> TablaSimbolos -> (Int, TablaSimbolos, Bool)
insertarOBuscar lexem ts =
    case tablaLocal ts of
        -- Sin tabla local: comportamiento original sobre la global
        Nothing ->
            case buscar lexem (tablaGlobal ts) of
                Just pos -> (pos, ts, False)
                Nothing  ->
                    let (pos, tg') = insertarEnTabla lexem (tablaGlobal ts)
                    in (pos, ts { tablaGlobal = tg' }, True)

        -- Con tabla local: buscar primero en local, luego en global
        -- Si no está en ninguna, insertar en la local
        Just tl ->
            case buscar lexem tl of
                Just pos -> (negate pos, ts, False)   -- posición local: negativa
                Nothing  ->
                    case buscar lexem (tablaGlobal ts) of
                        Just pos -> (pos, ts, False)  -- posición global: positiva
                        Nothing  ->
                            let (pos, tl') = insertarEnTabla lexem tl
                            in (negate pos, ts { tablaLocal = Just tl' }, True)

insertarEnTabla :: String -> Tabla -> (Int, Tabla)
insertarEnTabla lexem (Tabla lista) =
    let pos  = length lista
        fila = FilaTS { lexemaTS = lexem, infoTS = infoSemInicial }
    in (pos, Tabla (lista ++ [fila]))

-- Devuelve la posición del lexema Just posición, Nothing si no
buscar :: String -> Tabla -> Maybe Int
buscar iLexema (Tabla lista) = fila lista 0
  where
    fila []     _ = Nothing
    fila (x:xs) i
        | lexemaTS x == iLexema  = Just i
        | otherwise              = fila xs (i + 1)

lexema :: Int -> TablaSimbolos -> Maybe String
lexema i (TablaSimbolos global Nothing)         | i<0 = Nothing
                                                | otherwise = lexemaTabla i global
lexema i (TablaSimbolos global (Just local))    | i<0 = lexemaTabla (i*(-1)-1) local
                                                | otherwise = lexemaTabla i global

getInfo :: Int -> TablaSimbolos -> Maybe InfoSem
getInfo i (TablaSimbolos global Nothing)            | i<0 = Nothing
                                                    | otherwise = getInfoTabla i global
getInfo i (TablaSimbolos global (Just local))       | i<0 = getInfoTabla (i*(-1)-1) local
                                                    | otherwise = getInfoTabla i global

setInfo :: Int -> InfoSem -> TablaSimbolos -> TablaSimbolos
setInfo i info (TablaSimbolos global Nothing)           | i<0 = TablaSimbolos global Nothing
                                                        | otherwise = TablaSimbolos (setInfoTabla i info global) Nothing

setInfo i info (TablaSimbolos global (Just local))      | i<0 = TablaSimbolos global (Just (setInfoTabla (i*(-1)-1) info local))
                                                        | otherwise = TablaSimbolos (setInfoTabla i info global) (Just local)

updateInfo :: Int -> (InfoSem -> InfoSem) -> TablaSimbolos -> TablaSimbolos
updateInfo i f (TablaSimbolos global Nothing)           | i<0 = TablaSimbolos global Nothing
                                                        | otherwise = TablaSimbolos (updateInfoTabla i f global) Nothing

updateInfo i f (TablaSimbolos global (Just local))      | i<0 = TablaSimbolos global (Just (updateInfoTabla (i*(-1)-1) f local))
                                                        | otherwise = TablaSimbolos (updateInfoTabla i f global) (Just local)

----------------------
--Funciones auxiliares
----------------------

-- Devuelve el léxema de la posición i si existe, si no, Nothing
lexemaTabla :: Int -> Tabla-> Maybe String
lexemaTabla i (Tabla lista)
    | i < length lista = Just (lexemaTS (lista !! i))
    | otherwise        = Nothing

-- Devuelve la info semántica de la posición i
getInfoTabla :: Int -> Tabla-> Maybe InfoSem
getInfoTabla i (Tabla lista)
    | i < length lista = Just (infoTS (lista !! i))
    | otherwise        = Nothing

-- Actualiza la info semántica de la posición i
setInfoTabla :: Int -> InfoSem -> Tabla -> Tabla
setInfoTabla i info (Tabla lista)
    | i < length lista =
        let (antes, fila:despues) = splitAt i lista
        in Tabla (antes ++ [fila { infoTS = info }] ++ despues)
    | otherwise = Tabla lista

-- Actualiza un campo concreto sin tocar el resto
-- Útil para las acciones semánticas que solo conocen un atributo
-- Solución extraña pero simple
updateInfoTabla :: Int -> (InfoSem -> InfoSem) -> Tabla-> Tabla
updateInfoTabla i f (Tabla lista)
    | i < length lista =
        let (antes, fila:despues) = splitAt i lista
        in Tabla (antes ++ [fila { infoTS = f (infoTS fila) }] ++ despues)
    | otherwise = Tabla lista

