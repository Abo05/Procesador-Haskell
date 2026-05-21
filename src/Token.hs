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


identificadorACode :: String -> Token
identificadorACode "let" = TkLet
identificadorACode "int" = TkInt
identificadorACode "string" = TkString
identificadorACode "boolean" = TkBoolean
identificadorACode "float" = TkFloat
identificadorACode "void" = TkVoid
identificadorACode "if" = TkIf
identificadorACode "else" = TkElse
identificadorACode "function" = TkFunction
identificadorACode "read" = TkRead
identificadorACode "write" = TkWrite
identificadorACode "return" = TkReturn
-- TODO: Hacer que el int sea la posición en la tabla de símbolos
identificadorACode id = TkIdentificador 0

