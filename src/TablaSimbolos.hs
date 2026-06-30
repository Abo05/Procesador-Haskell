module TablaSimbolos where

import Simbolos (TipoSem(..), InfoSem(..), infoSemInicial, tamanioTipo, tipoAString, mostrarParametros)

data FilaTS = FilaTS
    { lexemaTS :: String
    , infoTS   :: InfoSem
    } deriving (Show)

newtype Tabla = Tabla [FilaTS] deriving (Show)

data TablaSimbolos = TablaSimbolos
    { tablaGlobal           :: Tabla
    , tablaLocal            :: Maybe Tabla   -- Nothing si no hay función activa
    , zonaDecl              :: Bool
    , desplazamientoGlobal  :: Int
    , desplazamientoLocal   :: Int
    , numTablas             :: Int
    } deriving (Show)

tablaInicial :: TablaSimbolos
tablaInicial = TablaSimbolos
    { tablaGlobal           = Tabla []
    , tablaLocal            = Nothing
    , zonaDecl              = False
    , desplazamientoGlobal  = 0
    , desplazamientoLocal   = 0
    , numTablas             = 1
    }

-- Crea una tabla local vacía al entrar en una función
crearTablaLocal :: TablaSimbolos -> TablaSimbolos
crearTablaLocal ts = ts 
    { tablaLocal            = Just (Tabla [])
    , desplazamientoLocal   = 0
    , numTablas             = numTablas ts + 1}

-- Destruye la tabla local al salir de la función
liberarTablaLocal :: TablaSimbolos -> TablaSimbolos
liberarTablaLocal ts = ts { tablaLocal = Nothing, desplazamientoLocal = 0 }

-- Devuelve (posición, tabla actualizada, fue insertado)
-- Posiciones positivas: tabla global
-- Posiciones negativas: tabla local (convenio del enunciado)
insertarOBuscar :: String -> TablaSimbolos -> (Int, TablaSimbolos)
insertarOBuscar lexem ts =
    case tablaLocal ts of
        Nothing ->
            case buscar lexem (tablaGlobal ts) of
                Just pos -> (pos, ts)
                Nothing  ->
                    -- No existe: si estamos declarando, tipo lo decidirá luego AccBLet;
                    -- si no, se declara implícitamente como entero global aquí mismo
                    let (pos, tg') = insertarEnTabla lexem (tablaGlobal ts)
                        ts'        = ts { tablaGlobal = tg' }
                        ts''       = if zonaDecl ts
                                        then ts'  -- en declaración explícita, el tipo lo pondrá AccBLet
                                        else asignarTipoYDespl pos TipoEntero ts'  -- implícita: entero
                    in (pos, ts'')

        Just tl ->
            case buscar lexem tl of
                Just pos -> (negate (pos + 1), ts)
                Nothing  ->
                    case buscar lexem (tablaGlobal ts) of
                        Just pos -> (pos, ts)
                        Nothing  ->
                            -- No existe ni en local ni en global
                            if zonaDecl ts
                                then
                                    -- Declaración explícita local
                                    let (pos, tl') = insertarEnTabla lexem tl
                                    in (negate (pos + 1), ts { tablaLocal = Just tl' })
                                else
                                    -- Uso implícito dentro de función: según el enunciado,
                                    -- "se considera variable GLOBAL entera", no local
                                    let (pos, tg') = insertarEnTabla lexem (tablaGlobal ts)
                                        ts'        = ts { tablaGlobal = tg' }
                                        ts''       = asignarTipoYDespl pos TipoEntero ts'
                                    in (pos, ts'')

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
lexema i (TablaSimbolos global Nothing _ _ _ _)      | i<0 = Nothing
                                                     | otherwise = lexemaTabla i global
lexema i (TablaSimbolos global (Just local) _ _ _ _) | i<0 = lexemaTabla (i*(-1)-1) local
                                                     | otherwise = lexemaTabla i global

getInfo :: Int -> TablaSimbolos -> Maybe InfoSem
getInfo i (TablaSimbolos global Nothing _ _ _ _)      | i<0 = Nothing
                                                      | otherwise = getInfoTabla i global
getInfo i (TablaSimbolos global (Just local) _ _ _ _) | i<0 = getInfoTabla (i*(-1)-1) local
                                                      | otherwise = getInfoTabla i global

setInfo :: Int -> InfoSem -> TablaSimbolos -> TablaSimbolos
setInfo i info (TablaSimbolos global Nothing decl dG dL nT)         | i<0 = TablaSimbolos global Nothing decl dG dL nT
                                                                    | otherwise = TablaSimbolos (setInfoTabla i info global) Nothing decl dG dL nT

setInfo i info (TablaSimbolos global (Just local) decl dG dL nT)    | i<0 = TablaSimbolos global (Just (setInfoTabla (i*(-1)-1) info local)) decl dG dL nT
                                                                    | otherwise = TablaSimbolos (setInfoTabla i info global) (Just local) decl dG dL nT

updateInfo :: Int -> (InfoSem -> InfoSem) -> TablaSimbolos -> TablaSimbolos
updateInfo i f (TablaSimbolos global Nothing decl dG dL nT)         | i<0 = TablaSimbolos global Nothing decl dG dL nT
                                                                    | otherwise = TablaSimbolos (updateInfoTabla i f global) Nothing decl dG dL nT

updateInfo i f (TablaSimbolos global (Just local) decl dG dL nT)    | i<0 = TablaSimbolos global (Just (updateInfoTabla (i*(-1)-1) f local)) decl dG dL nT
                                                                    | otherwise = TablaSimbolos (updateInfoTabla i f global) (Just local) decl dG dL nT

asignarTipoYDespl :: Int -> TipoSem -> TablaSimbolos -> TablaSimbolos
asignarTipoYDespl pos t ts
    | pos < 0 =
        let d   = desplazamientoLocal ts
            ts' = updateInfo pos (\i -> i { tipo = t, despl = d }) ts
        in ts' { desplazamientoLocal = d + tamanioTipo t }
    | otherwise =
        let d   = desplazamientoGlobal ts
            ts' = updateInfo pos (\i -> i { tipo = t, despl = d }) ts
        in ts' { desplazamientoGlobal = d + tamanioTipo t }
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

formatearTabla :: Tabla -> String
formatearTabla (Tabla filas) = concatMap formatearFila filas
  where
    formatearFila f =
        let info   = infoTS f
            base   = [ "* LEXEMA : '" ++ lexemaTS f ++ "'"
                     , "    +Tipo: " ++ tipoAString (tipo info)
                     , "    +Desplazamiento: " ++ show (despl info)
                     ]
            extra  = if tipo info == TipoFuncion
                        then [ "    +TipoRet: " ++ tipoAString (tipoRet info)
                             , "    +NumParametros: " ++ show (numParametros info)
                             , "    +Parametros: " ++ mostrarParametros (parametros info)
                             ]
                        else []
        in unlines (base ++ extra)

