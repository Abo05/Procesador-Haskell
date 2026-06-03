module Alex where

--Exportamos solo lo que se utiliza
import Data.Char (isSpace, isDigit, isAlpha, digitToInt, isAlphaNum) 

import Token    (Token(..), identificadorACode) 
import GErrores  (GError, nuevaLinea, registrarError, CodErr (..)) 
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
-- No nos vale con ver que el token es TkIdentificador, 
-- pues tenemos que ver si el identificador es nuevo o no
type TsMod = Bool
type Resultado = (Maybe Token, String, GError, TablaSimbolos, TsMod)

getToken :: String -> GError -> TablaSimbolos -> Resultado
getToken fich ge ts = estadoIn fich ge ts

-- Mueve el fichero hasta el siguiente caracter no alfabetico (Es una mickey herramienta que usaremos más adelante)
getNextNoAlfa :: String -> String
getNextNoAlfa [] = []
getNextNoAlfa fich@(c:cs) 
                | isAlpha c = getNextNoAlfa cs
                | otherwise = fich

------------------------
-- Todos los estados devuelven False en TsMod salvo estadoIdent (estado identificador)
-- (los demás no tocan la tabla de símbolos)


--Devuelvo el Token, gestor de errores, tabla de sismbolos y el string que queda después de haber leído el token
estadoIn :: String -> GError -> TablaSimbolos -> Resultado
estadoIn [] ge ts = (Just TkEof, [], ge, ts, False)

estadoIn (c:cs) ge ts
                | c == '\n'             = estadoIn cs (nuevaLinea ge) ts
                | isSpace c             = estadoIn cs ge ts
                | isDigit c             = estadoNum cs ge ts (digitToInt c)
                | isAlpha c || c == '_' = estadoIdent cs ge ts [c]
                | c == '"'              = estadoCad cs ge ts "" 0
                | c == '-'              = estadoMenos cs ge ts
                | c == '>'              = estadoMayor cs ge ts
                | c == '&'              = estadoAnd cs ge ts
                | c == '/'              = estadoInCom cs ge ts
                | c == '('              = (Just TkParentesisA,  cs, ge, ts, False)
                | c == ')'              = (Just TkParentesisC,  cs, ge, ts, False)
                | c == '+'              = (Just TkSuma,         cs, ge, ts, False)
                | c == ';'              = (Just TkPuntoComa,    cs, ge, ts, False)
                | c == '{'              = (Just TkLlaveA,       cs, ge, ts, False)
                | c == '}'              = (Just TkLlaveC,       cs, ge, ts, False)
                | c == ','              = (Just TkComa,         cs, ge, ts, False)
                | c == '!'              = (Just TkNot,          cs, ge, ts, False)
                | c == '='              = (Just TkAsignacion,   cs, ge, ts, False)
                | otherwise             =
                    let ge'             = registrarError ErrCarNoEsp ge
                    in (Nothing, cs, ge', ts, False)

-----------------------------------

estadoNum :: String -> GError -> TablaSimbolos -> Int -> Resultado
estadoNum [] ge ts ne =
                    let (tok, ge') = genEntero ge ne
                    in (tok, [], ge', ts, False)

estadoNum (c:cs) ge ts ne
    | isDigit c      = estadoNum cs ge ts (ne * 10 + digitToInt c)
    | c == '.'       = estadoPuntoDec cs ge ts ne
    | isAlpha c      = (Nothing, getNextNoAlfa (c:cs), (registrarError ErrNumInv ge), ts, False)
    | otherwise      = 
                    let (tok, ge') = genEntero ge ne
                    in (tok, cs, ge', ts, False)

genEntero :: GError -> Int -> (Maybe Token, GError)
genEntero ge ne
    | ne > maxEntero = (Nothing, registrarError ErrMaxEnt ge)
    | otherwise      = (Just (TkEntero ne), ge)

------------------------------------------------------

estadoPuntoDec :: String -> GError -> TablaSimbolos -> Int -> Resultado
estadoPuntoDec [] ge ts _ =
    (Nothing, [], registrarError ErrNoDec ge, ts, False)

estadoPuntoDec (c:cs) ge ts ne
    | isDigit c = estadoReal cs ge ts ne (fromIntegral (digitToInt c)) 1
    | otherwise = (Nothing, (getNextNoAlfa (c:cs)), registrarError ErrNumInv ge, ts, False)

------------------------------------------------------

-- Recibe además, la parte entera, la parte decimal y el número de decimales del número
estadoReal :: String -> GError -> TablaSimbolos -> Int -> Float -> Int -> Resultado
estadoReal [] ge ts ne dec ndec =
    let (tok, ge') = genReal ge ne dec ndec
    in (tok, [], ge', ts, False)

estadoReal (c:cs) ge ts ne dec ndec
    | isDigit c = estadoReal cs ge ts ne (dec * 10 + fromIntegral (digitToInt c)) (ndec + 1)
    | isAlpha c = (Nothing, getNextNoAlfa cs, (registrarError ErrNumInv ge), ts, False)
    | otherwise = 
                    let (tok, ge') = genReal ge ne dec ndec
                    in (tok, (c:cs), ge', ts, False)

genReal :: GError -> Int -> Float -> Int -> (Maybe Token, GError)
genReal ge ne dec ndec =
    let valor = fromIntegral ne + dec / (10.0 ^ ndec)
    in if valor > maxReal
        then (Nothing, registrarError ErrMaxReal ge)
        else (Just (TkReal valor), ge)

------------------------------------------------------

-- Unico estado que puede insertar en la tabla de símbolos
estadoIdent :: String -> GError -> TablaSimbolos -> String -> Resultado
estadoIdent [] ge ts lexem = resolverIdent [] ge ts lexem

estadoIdent (c:cs) ge ts lexem
    | isAlphaNum c || c == '_' = estadoIdent cs ge ts (lexem ++ [c])
    | otherwise                = resolverIdent (c:cs) ge ts lexem

resolverIdent :: String -> GError -> TablaSimbolos -> String -> Resultado
resolverIdent resto ge ts lexem =
    case identificadorACode lexem of
        Just tk ->
            -- Palabra reservada: no toca la TS
            (Just tk, resto, ge, ts, False)
        Nothing ->
            -- Identificador: insertar o buscar
            let (pos, ts', insertado) = insertarOBuscar lexem ts
            in (Just (TkIdentificador pos), resto, ge, ts', insertado)

------------------------------------------------------

estadoCad :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estadoCad [] ge ts _ _ =
    (Nothing, [], registrarError ErrCadNoCer ge, ts, False)
estadoCad (c:cs) ge ts lexem cont
    | c == '"'  =
        if cont >= maxCadena
            then (Nothing, cs, registrarError ErrMaxCad ge, ts, False)
            else (Just (TkCadena lexem), cs, ge, ts, False)
    | c == '\\' = estadoCarEsp cs ge ts lexem cont
    | c == '\t' = (Nothing, cs, registrarError ErrTab ge, ts, False)
    | c == '\n' = (Nothing, cs, registrarError ErrCadNoCer (nuevaLinea ge), ts, False)
    | otherwise = estadoCad cs ge ts (lexem ++ [c]) (cont + 1)

------------------------------------------------------

estadoCarEsp :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estadoCarEsp [] ge ts _ _ =
    (Nothing, [], registrarError ErrEscNoVal ge, ts, False)
estadoCarEsp (c:cs) ge ts lexem cont
    | c == 'n'  = estadoCad cs ge ts (lexem ++ "\\n") (cont + 1)
    | c == 't'  = estadoCad cs ge ts (lexem ++ "\\t") (cont + 1)
    | otherwise = (Nothing, c:cs, registrarError ErrEscNoVal ge, ts, False)

------------------------------------------------------

estadoMenos :: String -> GError -> TablaSimbolos -> Resultado
estadoMenos [] ge ts = (Just TkResta, [], ge, ts, False)
estadoMenos (c:cs) ge ts
    | c == '-'  = (Just TkAutodecremento, cs, ge, ts, False)
    | otherwise = (Just TkResta, c:cs, ge, ts, False)

------------------------------------------------------

estadoMayor :: String -> GError -> TablaSimbolos -> Resultado
estadoMayor [] ge ts = (Just TkMayor, [], ge, ts, False)
estadoMayor (c:cs) ge ts
    | c == '='  = (Just TkMayorIgual, cs, ge, ts, False)
    | otherwise = (Just TkMayor, c:cs, ge, ts, False)

------------------------------------------------------

estadoAnd :: String -> GError -> TablaSimbolos -> Resultado
estadoAnd [] ge ts =
    (Nothing, [], registrarError ErrAndInc ge, ts, False)
estadoAnd (c:cs) ge ts
    | c == '&'  = (Just TkAnd, cs, ge, ts, False)
    | otherwise = (Nothing, c:cs, registrarError ErrAndInc ge, ts, False)

------------------------------------------------------

estadoInCom :: String -> GError -> TablaSimbolos -> Resultado
estadoInCom [] ge ts =
    (Nothing, [], registrarError ErrComen ge, ts, False)
estadoInCom (c:cs) ge ts
    | c == '/'  = estadoComen cs ge ts
    | otherwise = (Nothing, c:cs, registrarError ErrComen ge, ts, False)

------------------------------------------------------

estadoComen :: String -> GError -> TablaSimbolos -> Resultado
estadoComen [] ge ts = (Just TkEof, [], ge, ts, False)
estadoComen (c:cs) ge ts
    | c == '\n' = estadoIn cs (nuevaLinea ge) ts
    | otherwise = estadoComen cs ge ts
------------------------------------------------------

