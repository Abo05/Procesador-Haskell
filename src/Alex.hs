module Alex where
--TODO: Tengo que poner que exporto, por pereza ahora exporta todo

import Data.Char --   (isSpace, isDigit, isAlpha)
import Token     --   (Token(..), identificadorACode) 
import GErrores  --      (Error(..), mensajeError) 
import TablaSimbolos

-- Constantes
maxEntero :: Int
maxEntero = 32767

maxReal :: Float
maxReal = 1.0e38

maxCadena = 64

--TODO: Hay que llevarlo continuamente y copiarlos, ver si es posible otra solución
type Resultado = (Maybe Token, String, GError, TablaSimbolos)

-- TODO: Terminar la función y ver como relacionarla a lo demás
getToken :: String -> GError -> TablaSimbolos -> Resultado
getToken fich ge ts = estado0 fich ge ts

------------------------

--Devuelvo el Token, gestor de errores, tabla de sismbolos y el string que queda después de haber leído el token
estado0 :: String -> GError -> TablaSimbolos -> Resultado
estado0 [] ge ts = (Just TkEof, [], ge, ts)

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
                | c == '('  = (Just TkParentesisA,  cs, ge, ts)
                | c == ')'  = (Just TkParentesisC,  cs, ge, ts)
                | c == '+'  = (Just TkSuma,         cs, ge, ts)
                | c == ';'  = (Just TkPuntoComa,    cs, ge, ts)
                | c == '{'  = (Just TkLlaveA,       cs, ge, ts)
                | c == '}'  = (Just TkLlaveC,       cs, ge, ts)
                | c == ','  = (Just TkComa,         cs, ge, ts)
                | c == '!'  = (Just TkNot,          cs, ge, ts)
                | c == '='  = (Just TkAsignacion,   cs, ge, ts)
                | otherwise =
                    let ge' = registrarError 11 ge
                    in (Nothing, cs, ge', ts)

-----------------------------------

estado1 :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estado1 [] ge ts _ _ = (Just TkEof, [], ge, ts)
estado1 (c:cs) ge ts _ _= (Nothing,cs,ge,ts)

-----------------------------------

estado2 :: String -> GError -> TablaSimbolos -> Resultado
estado2 [] ge ts = (Just TkEof, [], ge, ts)
estado2 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado3 :: String -> GError -> TablaSimbolos -> Resultado
estado3 [] ge ts = (Just TkEof, [], ge, ts)
estado3 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado4 :: String -> GError -> TablaSimbolos -> String -> Resultado
estado4 [] ge ts _ = (Just TkEof, [], ge, ts)
estado4 (c:cs) ge ts _ = (Nothing,cs,ge,ts)

-----------------------------------

estado5 :: String -> GError -> TablaSimbolos -> String -> Int -> Resultado
estado5 [] ge ts _ _ = (Just TkEof, [], ge, ts)
estado5 (c:cs) ge ts _ _ = (Nothing,cs,ge,ts)

-----------------------------------

estado6 :: String -> GError -> TablaSimbolos -> Resultado
estado6 [] ge ts = (Just TkEof, [], ge, ts)
estado6 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado7 :: String -> GError -> TablaSimbolos -> Resultado
estado7 [] ge ts = (Just TkEof, [], ge, ts)
estado7 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado8 :: String -> GError -> TablaSimbolos -> Resultado
estado8 [] ge ts = (Just TkEof, [], ge, ts)
estado8 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado9 :: String -> GError -> TablaSimbolos -> Resultado
estado9 [] ge ts = (Just TkEof, [], ge, ts)
estado9 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado10 :: String -> GError -> TablaSimbolos -> Resultado
estado10 [] ge ts = (Just TkEof, [], ge, ts)
estado10 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado11 :: String -> GError -> TablaSimbolos -> Resultado
estado11 [] ge ts = (Just TkEof, [], ge, ts)
estado11 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado12 :: String -> GError -> TablaSimbolos -> Resultado
estado12 [] ge ts = (Just TkEof, [], ge, ts)
estado12 (c:cs) ge ts = (Nothing,cs,ge,ts)

-----------------------------------

estado13 :: String -> GError -> TablaSimbolos -> Resultado
estado13 [] ge ts = (Just TkEof, [], ge, ts)
estado13 (c:cs) ge ts = (Nothing,cs,ge,ts)

