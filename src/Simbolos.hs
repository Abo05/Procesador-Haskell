module Simbolos where

import Token (Token(..))

-- Representan los posibles símbolos que pueden estar en la pila
data Simbolo
    = Terminal Token
    | NoTerminal NoTerminal
    | Dollar    --Es el símbolo de final de pila
    deriving (Show, Eq)

--No terminales de la gramática
data NoTerminal
    = A | B | C | D | E | E1 | F | G | H | I | K | L | O
    | P | Q | R | R1 | S | T | U | U1 | V | W | X | Y | Z
    deriving (Show, Eq, Ord, Enum, Bounded)

--TODO:Son los que teníamos en el otro pdl
-- Para los mensajes de error de no terminales
msgErrorNoTerminal :: NoTerminal -> String
msgErrorNoTerminal A  = "Se esperaba leer los parámetros de la función"
msgErrorNoTerminal B  = "Se esperaba una sentencia: 'let', 'if', '{', identificador, 'read', 'write' o 'return'"
msgErrorNoTerminal C  = "Se esperaba una sentencia: 'let', 'if', '{', identificador, 'read', 'write' o 'return'"
msgErrorNoTerminal D  = "Se esperaba leer un else if o el inicio de un bloque"
msgErrorNoTerminal E  = "Expresión formada por sumas, restas, ands... no terminada u operación no conocida"
msgErrorNoTerminal E1 = "Expresión formada por sumas, restas, ands... no terminada u operación no conocida"
msgErrorNoTerminal F  = "Se esperaba leer la declaración de una función"
msgErrorNoTerminal G  = "Se esperaba leer la asignación de una variable o la llamada a una función"
msgErrorNoTerminal H  = "Se esperaba leer el tipo devuelto por la función: int, void, boolean, float o string"
msgErrorNoTerminal I  = "Se esperaba leer un else o una sentencia válida"
msgErrorNoTerminal K  = "Lista de parámetros de la función no terminada"
msgErrorNoTerminal L  = "Lista de parámetros a la llamada de la función no terminada"
msgErrorNoTerminal O  = "Expresión no terminada o con un operando no conocido"
msgErrorNoTerminal P  = "Inicio de sentencia no válido, debe ser function, let, ..."
msgErrorNoTerminal Q  = "Lista de parámetros de la llamada a la función no terminada o no válida"
msgErrorNoTerminal R  = "Se esperaba una expresión"
msgErrorNoTerminal R1 = "Se esperaba una expresión"
msgErrorNoTerminal S  = "Se esperaba leer la asignación de una variable, la llamada a una función o return"
msgErrorNoTerminal T  = "Se esperaba leer un tipo de dato: int, float, boolean o string"
msgErrorNoTerminal U  = "Se esperaba una expresión"
msgErrorNoTerminal U1 = "Se esperaba una expresión"
msgErrorNoTerminal V  = "Se esperaba una expresión"
msgErrorNoTerminal W  = "Se esperaba una expresión"
msgErrorNoTerminal X  = "Expresión no válida en el return"
msgErrorNoTerminal Y  = "Se esperaba leer el inicio de un bloque o una sentencia válida"
msgErrorNoTerminal Z  = "Se esperaba el cuerpo del 'if': un bloque '{...}' o una instrucción simple"

-- Para los mensajes de error de terminales
msgErrorTerminal :: Token -> String
msgErrorTerminal (TkEntero n)        = "un número entero (" ++ show n ++ ")"
msgErrorTerminal (TkReal f)          = "un número real (" ++ show f ++ ")"
msgErrorTerminal (TkCadena s)        = "una cadena (\"" ++ s ++ "\")"
msgErrorTerminal TkTrue              = "la palabra reservada true"
msgErrorTerminal TkFalse             = "la palabra reservada false"
msgErrorTerminal TkParentesisA       = "un paréntesis de apertura"
msgErrorTerminal TkParentesisC       = "un paréntesis de cierre"
msgErrorTerminal TkSuma              = "el símbolo más (+)"
msgErrorTerminal TkResta             = "el símbolo menos (-)"
msgErrorTerminal TkMayor             = "el símbolo mayor (>)"
msgErrorTerminal TkMayorIgual        = "el símbolo mayor o igual (>=)"
msgErrorTerminal TkNot               = "el símbolo not (!)"
msgErrorTerminal TkAnd               = "el símbolo and (&&)"
msgErrorTerminal TkAutodecremento    = "el símbolo autodecremento (--)"
msgErrorTerminal TkAsignacion        = "el símbolo de asignación (=)"
msgErrorTerminal (TkIdentificador i) = "un identificador (pos " ++ show i ++ ")"
msgErrorTerminal TkLet               = "la palabra reservada let"
msgErrorTerminal TkInt               = "la palabra reservada int"
msgErrorTerminal TkString            = "la palabra reservada string"
msgErrorTerminal TkBoolean           = "la palabra reservada boolean"
msgErrorTerminal TkFloat             = "la palabra reservada float"
msgErrorTerminal TkVoid              = "la palabra reservada void"
msgErrorTerminal TkPuntoComa         = "un punto y coma (;)"
msgErrorTerminal TkLlaveA            = "una llave de apertura ({)"
msgErrorTerminal TkLlaveC            = "una llave de cierre (})"
msgErrorTerminal TkIf                = "la palabra reservada if"
msgErrorTerminal TkElse              = "la palabra reservada else"
msgErrorTerminal TkComa              = "una coma (,)"
msgErrorTerminal TkFunction          = "la palabra reservada function"
msgErrorTerminal TkRead              = "la palabra reservada read"
msgErrorTerminal TkReturn            = "la palabra reservada return"
msgErrorTerminal TkWrite             = "la palabra reservada write"
msgErrorTerminal TkEof               = "fin de fichero"

