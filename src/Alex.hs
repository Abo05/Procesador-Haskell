module Alex where

import Data.Char    (isSpace, isDigit, isAlpha)
import Token        (Token(..), lexemaAToken) 
import Error        (Error(..), mensajeError) 

-- Constantes
maxEntero :: Int
maxEntero = 32767

maxReal :: Float
maxReal = 1.0e38

maxCadena = 64

-- TODO: Terminar la función y ver como relacionarla a lo demás
getTokens :: String -> [Maybe Token]
getTokens fich = getTokensAux fich 0
    where
        -- Caso de que el fichero ha acabado y lo ha hecho bien
        getTokensAux "" 0 = [Just (TkEof, Nil)]
        getTokensAux (c : r) 0
            | isSpace c = getTokensAux r 0
            | isDigit c = getTokensAux r 1
            | (isAlpha c || c == '_') = getTokensAux r 4
            | (c == '"') = getTokensAux r 5
            | (c == '-') = getTokensAux r 7
            | (c == '>') = getTokensAux r 8
            | (c == '&') = getTokensAux r 9
            | (c == '/') = getTokensAux r 10
            | (c == ')') = Just (TkParentesisA, Nil) : getTokensAux r 0
            | otherwise = Nothing : getTokensAux r 0 {- En caso de error devolvemos Nothing y volvemos al caso cero -}
        -- Error pero todavia nos quedan caracteres que podemos leer, así que indicamos un error y continuamos
        getTokensAux (_ : r) _ = Nothing : getTokensAux r 0
        -- Error y no hay nada que leer, indicamos que ocurrio un error y que hemos leido el fin de fichero
        getTokensAux "" _ = [Nothing, Just (TkEof, Nil)]
