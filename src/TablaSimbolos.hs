module TablaSimbolos where

newtype TablaSimbolos = TS [String] deriving Show

tablaInicial :: TablaSimbolos
tablaInicial = TS[]

-- Devuelve (posición, tabla actualizada)
-- Si el lexema ya existe devuelve su posición sin modificar la tabla
-- Si no existe lo inserta al final y devuelve la nueva posición
insertarOBuscar :: String -> TablaSimbolos -> (Int, TablaSimbolos)
insertarOBuscar lex ts@(TS lista) =
    case buscar lex ts of
        Just pos -> (pos, ts)
        Nothing  ->
            let pos = length lista
            in (pos, TS (lista ++ [lex]))

-- Devuelve Just posición si existe, Nothing si no
buscar :: String -> TablaSimbolos -> Maybe Int
buscar lex (TS lista) = fila lista 0
  where
    fila []     _ = Nothing
    fila (x:xs) i
        | x == lex  = Just i
        | otherwise = fila xs (i + 1)
