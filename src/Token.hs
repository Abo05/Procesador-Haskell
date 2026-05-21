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
-- TODO: Hacer que el int sea la posición en la tabla de símbolos
identificadorACode id           = Nothing

