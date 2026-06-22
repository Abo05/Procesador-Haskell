module Alex where

--Importamos solo lo que se utiliza
import Data.Char (isSpace, isDigit, isAlpha, digitToInt, isAlphaNum) 

import Token    (Token(..), identificadorACode) 
import GErrores  (GError, nuevaLinea, registrarError, CodErr (..)) 
import TablaSimbolos (TablaSimbolos, insertarOBuscar)

-- Constantes
maxEntero :: Int
maxEntero = 32767

maxReal :: Float
maxReal = 1.0e38

maxCadena :: Int
maxCadena = 64

-- Tiene el token, el léxema del token, el gestor de errores y la tabla de símbolos
type TsMod = Bool
type Resultado = (Maybe Token, String, GError, TablaSimbolos)

getToken :: String -> GError -> TablaSimbolos -> Resultado
getToken fich ge ts = estadoIn fich ge ts

-- Mueve el fichero hasta el siguiente caracter no alfabetico (Es una mickey herramienta que usaremos más adelante)
getNextNoAlfa :: String -> String
getNextNoAlfa [] = []
getNextNoAlfa fich@(c:cs) 
                | isAlpha c = getNextNoAlfa cs
                | otherwise = fich

--Devuelvo el Token, gestor de errores, tabla de sismbolos y el string que queda después de haber leído el token
estadoIn :: String -> GError -> TablaSimbolos -> Resultado
estadoIn [] ge ts = (Just TkEof, [], ge, ts)

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
                | c == '('              = (Just TkParentesisA,  cs, ge, ts)
                | c == ')'              = (Just TkParentesisC,  cs, ge, ts)
                | c == '+'              = (Just TkSuma,         cs, ge, ts)
                | c == ';'              = (Just TkPuntoComa,    cs, ge, ts)
                | c == '{'              = (Just TkLlaveA,       cs, ge, ts)
                | c == '}'              = (Just TkLlaveC,       cs, ge, ts)
                | c == ','              = (Just TkComa,         cs, ge, ts)
                | c == '!'              = (Just TkNot,          cs, ge, ts)
                | c == '='              = (Just TkAsignacion,   cs, ge, ts)
                | otherwise             =
                    let ge'             = registrarError ErrCarNoEsp ge
                    in (Nothing, cs, ge', ts)

-----------------------------------

estadoNum :: String -> GError -> TablaSimbolos -> Int -> Resultado
estadoNum [] ge ts ne =
                    let (tok, ge') = genEntero ge ne
                    in (tok, [], ge', ts)

estadoNum (c:cs) ge ts ne
    | isDigit c      = estadoNum cs ge ts (ne * 10 + digitToInt c)
    | c == '.'       = estadoPuntoDec cs ge ts ne
    | isAlpha c      = (Nothing, getNextNoAlfa (c:cs), registrarError ErrNumInv ge, ts)
    | otherwise      = 
                    let (tok, ge') = genEntero ge ne
                    in (tok, c:cs, ge', ts)

genEntero :: GError -> Int -> (Maybe Token, GError)
genEntero ge ne
    | ne > maxEntero = (Nothing, registrarError ErrMaxEnt ge)
    | otherwise      = (Just (TkEntero ne), ge)

------------------------------------------------------

estadoPuntoDec :: String -> GError -> TablaSimbolos -> Int -> Resultado
estadoPuntoDec [] ge ts _ =
    (Nothing, [], registrarError ErrNoDec ge, ts)

estadoPuntoDec (c:cs) ge ts ne
    | isDigit c = estadoReal cs ge ts ne (fromIntegral (digitToInt c)) 1
    | otherwise = (Nothing, (getNextNoAlfa (c:cs)), registrarError ErrNumInv ge, ts)

------------------------------------------------------

-- Recibe además, la parte entera, la parte decimal y el número de decimales del número
estadoReal :: String -> GError -> TablaSimbolos -> Int -> Float -> Int -> Resultado
estadoReal [] ge ts ne dec ndec =
    let (tok, ge') = genReal ge ne dec ndec
    in (tok, [], ge', ts)

estadoReal (c:cs) ge ts ne dec ndec
    | isDigit c = estadoReal cs ge ts ne (dec * 10 + fromIntegral (digitToInt c)) (ndec + 1)
    | isAlpha c = (Nothing, getNextNoAlfa cs, registrarError ErrNumInv ge, ts)
    | otherwise = 
                    let (tok, ge') = genReal ge ne dec ndec
                    in (tok, c:cs, ge', ts)

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
            (Just tk, resto, ge, ts)
        Nothing ->
            -- Identificador: insertar o buscar
            case insertarOBuscar lexem ts of
                (Nothing, ts') ->
                    -- Identificador no declarado fuera de zona de declaración
                    let ge' = registrarError ErrIdNoDecl ge
                    in (Nothing, resto, ge', ts')
                (Just pos, ts') ->
                    (Just (TkIdentificador pos), resto, ge, ts')

------------------------------------------------------

estadoCad :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estadoCad [] ge ts _ _ =
    (Nothing, [], registrarError ErrCadNoCer ge, ts)
estadoCad (c:cs) ge ts lexem cont
    | c == '"'  =
        if cont >= maxCadena
            then (Nothing, cs, registrarError ErrMaxCad ge, ts)
            else (Just (TkCadena lexem), cs, ge, ts)
    | c == '\\' = estadoCarEsp cs ge ts lexem cont
    | c == '\t' = (Nothing, cs, registrarError ErrTab ge, ts)
    | c == '\n' = (Nothing, cs, registrarError ErrCadNoCer (nuevaLinea ge), ts)
    | otherwise = estadoCad cs ge ts (lexem ++ [c]) (cont + 1)

------------------------------------------------------

estadoCarEsp :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estadoCarEsp [] ge ts _ _ =
    (Nothing, [], registrarError ErrEscNoVal ge, ts)
estadoCarEsp (c:cs) ge ts lexem cont
    | c == 'n'  = estadoCad cs ge ts (lexem ++ "\\n") (cont + 1)
    | c == 't'  = estadoCad cs ge ts (lexem ++ "\\t") (cont + 1)
    | otherwise = (Nothing, c:cs, registrarError ErrEscNoVal ge, ts)

------------------------------------------------------

estadoMenos :: String -> GError -> TablaSimbolos -> Resultado
estadoMenos [] ge ts = (Just TkResta, [], ge, ts)
estadoMenos (c:cs) ge ts
    | c == '-'  = (Just TkAutodecremento, cs, ge, ts)
    | otherwise = (Just TkResta, c:cs, ge, ts)

------------------------------------------------------

estadoMayor :: String -> GError -> TablaSimbolos -> Resultado
estadoMayor [] ge ts = (Just TkMayor, [], ge, ts)
estadoMayor (c:cs) ge ts
    | c == '='  = (Just TkMayorIgual, cs, ge, ts)
    | otherwise = (Just TkMayor, c:cs, ge, ts)

------------------------------------------------------

estadoAnd :: String -> GError -> TablaSimbolos -> Resultado
estadoAnd [] ge ts =
    (Nothing, [], registrarError ErrAndInc ge, ts)
estadoAnd (c:cs) ge ts
    | c == '&'  = (Just TkAnd, cs, ge, ts)
    | otherwise = (Nothing, c:cs, registrarError ErrAndInc ge, ts)

------------------------------------------------------

estadoInCom :: String -> GError -> TablaSimbolos -> Resultado
estadoInCom [] ge ts =
    (Nothing, [], registrarError ErrComen ge, ts)
estadoInCom (c:cs) ge ts
    | c == '/'  = estadoComen cs ge ts
    | otherwise = (Nothing, c:cs, registrarError ErrComen ge, ts)

------------------------------------------------------

estadoComen :: String -> GError -> TablaSimbolos -> Resultado
estadoComen [] ge ts = (Just TkEof, [], ge, ts)
estadoComen (c:cs) ge ts
    | c == '\n' = estadoIn cs (nuevaLinea ge) ts
    | otherwise = estadoComen cs ge ts
------------------------------------------------------

