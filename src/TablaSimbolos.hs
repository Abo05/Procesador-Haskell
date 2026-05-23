module TablaSimbolos where

newtype TablaSimbolos = TS [String] deriving Show

tablaInicial :: TablaSimbolos
tablaInicial = TS[]

-- Devuelve (posición, tabla actualizada, true si insertado)
-- Si el lexema ya existe devuelve su posición sin modificar la tabla
-- Si no existe lo inserta al final y devuelve la nueva posición
-- La tabla de simbolos no imprime en el fichero tablasímbolos, complicaría las cosas
insertarOBuscar :: String -> TablaSimbolos -> (Int, TablaSimbolos, Bool)
insertarOBuscar iLexema ts@(TS lista) =
    case buscar iLexema ts of
        Just pos -> (pos, ts, False)
        Nothing  ->
            let pos = length lista
            in (pos, TS (lista ++ [iLexema]), True)

-- Devuelve la posición del lexema Just posición, Nothing si no
buscar :: String -> TablaSimbolos -> Maybe Int
buscar iLexema (TS lista) = fila lista 0
  where
    fila []     _ = Nothing
    fila (x:xs) i
        | x == iLexema  = Just i
        | otherwise = fila xs (i + 1)

-- Devuelve el léxema de la posición i si existe, si no, Nothing
lexema :: Int -> TablaSimbolos -> Maybe String
lexema i (TS lista)
    | i < length lista = Just (lista !! i)
    | otherwise        = Nothing

