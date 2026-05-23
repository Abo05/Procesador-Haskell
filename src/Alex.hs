module Alex where
--TODO: Tengo que poner qué exporto, por pereza ahora exporta todo

import Data.Char --   (isSpace, isDigit, isAlpha)
import Token     --   (Token(..), identificadorACode) 
import GErrores  --      (Error(..), mensajeError) 
import TablaSimbolos

-- Constantes
maxEntero :: Int
maxEntero = 32767

maxReal :: Float
maxReal = 1.0e38

maxCadena :: Int
maxCadena = 64

-- Tiene las estructuras necesarias y el token devuelto.
-- Además, True si se ha insertado un léxema en la tabla de símbolos
-- No nos vale con token, pues tenemos que ver si el identificador es nuevo o no
type Resultado = (Maybe Token, String, GError, TablaSimbolos, Bool)

getToken :: String -> GError -> TablaSimbolos -> Resultado
getToken fich ge ts = estado0 fich ge ts

------------------------

--Devuelvo el Token, gestor de errores, tabla de sismbolos y el string que queda después de haber leído el token
estado0 :: String -> GError -> TablaSimbolos -> Resultado
estado0 [] ge ts = (Just TkEof, [], ge, ts, False)

estado0 (c:cs) ge ts
                | c == '\n'             = estado0 cs (nuevaLinea ge) ts
                | isSpace c             = estado0 cs ge ts
                | isDigit c             = estado1 cs ge ts [c] (digitToInt c)
                | isAlpha c || c == '_' = estado4 cs ge ts [c]
                | c == '"'              = estado5 cs ge ts "" 0
                | c == '-'              = estado7 cs ge ts
                | c == '>'              = estado8 cs ge ts
                | c == '&'              = estado9 cs ge ts
                | c == '/'              = estado10 cs ge ts
                | c == '('  = (Just TkParentesisA,  cs, ge, ts, False)
                | c == ')'  = (Just TkParentesisC,  cs, ge, ts, False)
                | c == '+'  = (Just TkSuma,         cs, ge, ts, False)
                | c == ';'  = (Just TkPuntoComa,    cs, ge, ts, False)
                | c == '{'  = (Just TkLlaveA,       cs, ge, ts, False)
                | c == '}'  = (Just TkLlaveC,       cs, ge, ts, False)
                | c == ','  = (Just TkComa,         cs, ge, ts, False)
                | c == '!'  = (Just TkNot,          cs, ge, ts, False)
                | c == '='  = (Just TkAsignacion,   cs, ge, ts, False)
                | otherwise =
                    let ge' = registrarError 11 ge
                    in (Nothing, cs, ge', ts, False)

--TODO:IMPORTANTE. A partir de aquí copiado de Claude. REVISAR
-----------------------------------
-- Estados 1-10: todos devuelven False en el Bool salvo estado4
-- (los demás no tocan la tabla de símbolos)

estado1 :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estado1 [] ge ts _ ne =
    (genEntero ge ne, [], ge, ts, False)
estado1 (c:cs) ge ts lex ne
    | isDigit c = estado1 cs ge ts (lex ++ [c]) (ne * 10 + digitToInt c)
    | c == '.'  = estado2 cs ge ts ne
    | otherwise = (genEntero ge ne, c:cs, ge, ts, False)

genEntero :: GError -> Int -> Maybe Token
genEntero ge ne
    | ne > maxEntero = Nothing   -- el error se registra en quien llama
    | otherwise      = Just (TkEntero ne)

------------------------------------------------------

estado2 :: String -> GError -> TablaSimbolos -> Int -> Resultado
estado2 [] ge ts _ =
    (Nothing, [], registrarError 2 ge, ts, False)
estado2 (c:cs) ge ts ne
    | isDigit c = estado3 cs ge ts ne (fromIntegral (digitToInt c)) 1
    | otherwise = (Nothing, c:cs, registrarError 2 ge, ts, False)

------------------------------------------------------

estado3 :: String -> GError -> TablaSimbolos -> Int -> Float -> Int -> Resultado
estado3 [] ge ts ne dec ndec =
    let (tok, ge') = genReal ge ne dec ndec
    in (tok, [], ge', ts, False)
estado3 (c:cs) ge ts ne dec ndec
    | isDigit c = estado3 cs ge ts ne (dec * 10 + fromIntegral (digitToInt c)) (ndec + 1)
    | otherwise =
        let (tok, ge') = genReal ge ne dec ndec
        in (tok, c:cs, ge', ts, False)

genReal :: GError -> Int -> Float -> Int -> (Maybe Token, GError)
genReal ge ne dec ndec =
    let valor = fromIntegral ne + dec / (10.0 ^ ndec)
    in if valor > maxReal
        then (Nothing, registrarError 24 ge)
        else (Just (TkReal valor), ge)

------------------------------------------------------

-- Estado 4: único que puede insertar en la tabla de símbolos
estado4 :: String -> GError -> TablaSimbolos -> String -> Resultado
estado4 [] ge ts lex = resolverIdent [] ge ts lex
estado4 (c:cs) ge ts lex
    | isAlphaNum c || c == '_' = estado4 cs ge ts (lex ++ [c])
    | otherwise                = resolverIdent (c:cs) ge ts lex

resolverIdent :: String -> GError -> TablaSimbolos -> String -> Resultado
resolverIdent resto ge ts lex =
    case identificadorACode lex of
        Just tk ->
            -- Palabra reservada: no toca la TS
            (Just tk, resto, ge, ts, False)
        Nothing ->
            -- Identificador: insertar o buscar
            let (pos, ts', insertado) = insertarOBuscar lex ts
            in (Just (TkIdentificador pos), resto, ge, ts', insertado)

------------------------------------------------------

estado5 :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estado5 [] ge ts _ _ =
    (Nothing, [], registrarError 5 ge, ts, False)
estado5 (c:cs) ge ts lex cont
    | c == '"'  =
        if cont >= maxCadena
            then (Nothing, cs, registrarError 28 ge, ts, False)
            else (Just (TkCadena lex), cs, ge, ts, False)
    | c == '\\' = estado6 cs ge ts lex cont
    | c == '\t' = (Nothing, cs, registrarError 4 ge, ts, False)
    | c == '\n' = (Nothing, cs, registrarError 5 (nuevaLinea ge), ts, False)
    | otherwise = estado5 cs ge ts (lex ++ [c]) (cont + 1)

------------------------------------------------------

estado6 :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estado6 [] ge ts _ _ =
    (Nothing, [], registrarError 6 ge, ts, False)
estado6 (c:cs) ge ts lex cont
    | c == 'n'  = estado5 cs ge ts (lex ++ "\n") (cont + 1)
    | c == 't'  = estado5 cs ge ts (lex ++ "\t") (cont + 1)
    | otherwise = (Nothing, c:cs, registrarError 6 ge, ts, False)

------------------------------------------------------

estado7 :: String -> GError -> TablaSimbolos -> Resultado
estado7 [] ge ts = (Just TkResta, [], ge, ts, False)
estado7 (c:cs) ge ts
    | c == '-'  = (Just TkAutodecremento, cs, ge, ts, False)
    | otherwise = (Just TkResta, c:cs, ge, ts, False)

------------------------------------------------------

estado8 :: String -> GError -> TablaSimbolos -> Resultado
estado8 [] ge ts = (Just TkMayor, [], ge, ts, False)
estado8 (c:cs) ge ts
    | c == '='  = (Just TkMayorIgual, cs, ge, ts, False)
    | otherwise = (Just TkMayor, c:cs, ge, ts, False)

------------------------------------------------------

estado9 :: String -> GError -> TablaSimbolos -> Resultado
estado9 [] ge ts =
    (Nothing, [], registrarError 9 ge, ts, False)
estado9 (c:cs) ge ts
    | c == '&'  = (Just TkAnd, cs, ge, ts, False)
    | otherwise = (Nothing, c:cs, registrarError 9 ge, ts, False)

------------------------------------------------------

estado10 :: String -> GError -> TablaSimbolos -> Resultado
estado10 [] ge ts =
    (Nothing, [], registrarError 10 ge, ts, False)
estado10 (c:cs) ge ts
    | c == '/'  = estado11 cs ge ts
    | otherwise = (Nothing, c:cs, registrarError 10 ge, ts, False)

------------------------------------------------------

estado11 :: String -> GError -> TablaSimbolos -> Resultado
estado11 [] ge ts = (Just TkEof, [], ge, ts, False)
estado11 (c:cs) ge ts
    | c == '\n' = estado0 cs (nuevaLinea ge) ts
    | otherwise = estado11 cs ge ts
------------------------------------------------------

