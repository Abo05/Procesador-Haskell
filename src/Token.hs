module Token where

-- Enumerado que contiene los tokens posibles, estos pueden tener un atributo
data Token 
    = TkEntero Int
    | TkReal Float
    | TkCadena String
    | TkTrue
    | TkFalse
    | TkParentesisA
    | TkParentesisC
    | TkSuma
    | TkResta
    | TkMayor
    | TkMayorIgual
    | TkNot
    | TkAnd
    | TkAutodecremento
    | TkAsignacion
    | TkIdentificador Int
    | TkLet
    | TkInt
    | TkString
    | TkBoolean
    | TkFloat
    | TkVoid
    | TkPuntoComa
    | TkLlaveA
    | TkLlaveC
    | TkIf
    | TkElse
    | TkComa
    | TkFunction
    | TkRead
    | TkWrite
    | TkEof
    | TkReturn
    deriving (Show, Eq)



-- Si devuelve Nothing, hay que buscar en la tabla de símbolos
identificadorACode :: String -> Maybe Token
identificadorACode "let"        = Just TkLet
identificadorACode "int"        = Just TkInt
identificadorACode "string"     = Just TkString
identificadorACode "boolean"    = Just TkBoolean
identificadorACode "float"      = Just TkFloat
identificadorACode "void"       = Just TkVoid
identificadorACode "if"         = Just TkIf
identificadorACode "else"       = Just TkElse
identificadorACode "function"   = Just TkFunction
identificadorACode "read"       = Just TkRead
identificadorACode "write"      = Just TkWrite
identificadorACode "return"     = Just TkReturn
identificadorACode _            = Nothing

-- Formato <Codigo,Atributo> para el fichero de tokens
formatToken :: Token -> String
formatToken tk = "<" ++ codigoTk tk ++ "," ++ atributoTk tk ++ ">"

codigoTk :: Token -> String
codigoTk (TkEntero _)        = "entero"
codigoTk (TkReal _)          = "real"
codigoTk (TkCadena _)        = "cadena"
codigoTk TkTrue              = "true"
codigoTk TkFalse             = "false"
codigoTk TkParentesisA       = "parentesisApertura"
codigoTk TkParentesisC       = "parentesisCierre"
codigoTk TkSuma              = "suma"
codigoTk TkResta             = "resta"
codigoTk TkMayor             = "mayor"
codigoTk TkMayorIgual        = "mayorIgual"
codigoTk TkNot               = "not"
codigoTk TkAnd               = "and"
codigoTk TkAutodecremento    = "autodecremento"
codigoTk TkAsignacion        = "asignacion"
codigoTk (TkIdentificador _) = "identificador"
codigoTk TkLet               = "let"
codigoTk TkInt               = "int"
codigoTk TkString            = "string"
codigoTk TkBoolean           = "boolean"
codigoTk TkFloat             = "float"
codigoTk TkVoid              = "void"
codigoTk TkPuntoComa         = "puntoComa"
codigoTk TkLlaveA            = "llaveApertura"
codigoTk TkLlaveC            = "llaveCierre"
codigoTk TkIf                = "if"
codigoTk TkElse              = "else"
codigoTk TkComa              = "coma"
codigoTk TkFunction          = "function"
codigoTk TkRead              = "read"
codigoTk TkWrite             = "write"
codigoTk TkReturn            = "return"
codigoTk TkEof               = "eof"

atributoTk :: Token -> String
atributoTk (TkEntero n)        = show n
atributoTk (TkReal f)          = show f
atributoTk (TkCadena s)        = s
atributoTk (TkIdentificador i) = show i
atributoTk _                   = ""

--Compara Tokens
mismoTipo :: Token -> Token -> Bool
mismoTipo (TkEntero _)          (TkEntero _)        = True
mismoTipo (TkReal _)            (TkReal _)          = True
mismoTipo (TkCadena _)          (TkCadena _)        = True
mismoTipo (TkIdentificador _)   (TkIdentificador _) = True
mismoTipo a                     b                   = a==b

