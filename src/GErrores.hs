module GErrores where
import Simbolos (NoTerminal(..))
import Token (Token(..))

--Guardamos la línea acutal del fichero y los errores encontrados
data GError = GError
    { linea :: Int
    , errores :: [Error]
    } deriving (Show)

--Guardamos la línea del error, el código de error y su mensaje
data Error = Error
    { errLinea :: Int
    , errCodigo :: CodErr
    , errMensaje :: String
    } deriving (Eq)

--Se crean los getters automaticamente al crear el data
instance Show Error where
    show e = "error en la línea " ++ show(errLinea e) ++ ": " ++ errMensaje e

--Inicializamos el gestor de errores en la línea 1 y sin errores
gErrorInicial :: GError
gErrorInicial = GError
    { linea     = 1
    , errores   = []
    }

-- Esto copía la variable, pero cambia la línea
nuevaLinea :: GError -> GError
nuevaLinea ge = ge
    { linea     = linea ge + 1
    }

-- Cada vez crea una nueva variable de GErrores para añadir el error
registrarError :: CodErr -> GError -> GError
registrarError cod ge =
        let err = Error
                { errLinea    = linea ge
                , errCodigo   = cod
                , errMensaje  = mensajeError cod
                }
        in ge { errores = err : errores ge }

registrarErrorAsin :: CodErr -> String -> GError -> GError
registrarErrorAsin cod msg ge = let err = Error
                                            { errLinea    = linea ge
                                            , errCodigo   = cod
                                            , errMensaje  = msg
                                            }
                                in ge {errores = err : errores ge}

hayErrores :: GError -> Bool
hayErrores ge = not (null(errores ge))

-- Los errores se acumularon en orden inverso
listarErrores :: GError -> [Error]
listarErrores ge = reverse (errores ge)

data CodErr 
             = ErrCarNoEsp
             | ErrNoDec
             | ErrTab
             | ErrCadNoCer
             | ErrEscNoVal
             | ErrAndInc
             | ErrComen
             | ErrCarDesc
             | ErrMaxEnt
             | ErrMaxReal
             | ErrMaxCad
             | ErrNumInv
             | ErrIdInv
             | ErrNoTerminal
             | ErrTerminal
             | ErrSemantico
             | ErrIdNoDecl
             deriving Eq

mensajeError :: CodErr -> String
mensajeError ErrCarNoEsp  = "carácter no esperado"
mensajeError ErrNoDec     = "número real con parte decimal faltante"
mensajeError ErrTab       = "carácter TAB no permitido en cadena"
mensajeError ErrCadNoCer  = "cadena no cerrado correctamente"
mensajeError ErrEscNoVal  = "secuencia de escape no válida"
mensajeError ErrAndInc    = "operador and incompleto"
mensajeError ErrComen     = "otra / esperada para inicio de comentario"
mensajeError ErrCarDesc   = "carácter desconocido"
mensajeError ErrMaxEnt    = "valor entero excede el máximo permitido"
mensajeError ErrMaxReal   = "valor real excede el máximo permitido"
mensajeError ErrMaxCad    = "cadena excede el número máximo de caracteres"
mensajeError ErrNumInv    = "carácter inesperado después de número"
mensajeError ErrIdInv     = "identificador con carácter no válido"
mensajeError ErrIdNoDecl = "identificador no declarado"
mensajeError _            = "este error no debería salir. El sintáctico llamó a esta función"

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
msgErrorNoTerminal PP  = "Se esperaba el inicio de una sentencia válida"

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
