module Simbolos where

import Token (Token(..))

-- Representan los posibles símbolos que pueden estar en la pila
data Simbolo
    = Terminal Token
    | NoTerminal NoTerminal
    | Accion Accion
    | Dollar    --Es el símbolo de final de pila
    deriving (Show, Eq)

--No terminales de la gramática
data NoTerminal
    = A | B | C | D | E | E1 | F | G | H | I | K | L | O
    | P | PP | Q | R | R1 | S | T | U | U1 | V | W | X | Y | Z
    deriving (Show, Eq, Ord, Enum, Bounded)

data Accion
    = AccER             -- {E.tipo = if(E1.tipo = void || R.tipo = E1.tipo) then R.tipo else tipo_error}
    | AccE1And          -- {E1.tipo = if(R.tipo = logico && E1_1.tipo € {logico, void}) then R.tipo else tipo_error}
    | AccE1Void         -- {E1.tipo = void} 
    | AccRU             -- {R.tipo = if(R1.tipo = void) then U.tipo else if(U.tipo = R1.tipo}) logico else tipo_error}
    | AccR1M            -- {U.tipo∈{real, entero} && R1_2 = void then U.tipo else tipo_error}
    | AccR1MI           -- {U.tipo∈{real, entero} && R1_2 = void then U.tipo else tipo_error}
    | AccR1Void         -- {R1.tipo = void}
    | AccUV             -- {U.tipo = if(U1.tipo = void || V.tipo = U1.tipo) then V.tipo else tipo_error} 
    | AccU1Suma         -- {U1.tipo = if(V.tipo € {entero, real} && U1_1.tipo = void || V.tipo = U1_1.tipo) then V.tipo else tipo_error}
    | AccU1Resta        -- {U1.tipo = if(V.tipo € {entero, real} && U1_1.tipo = void || V.tipo = U1_1.tipo) then V.tipo else tipo_error}
    | AccU1Void         -- {U1.tipo = void}
    | AccMasW           -- {V.tipo = if(W.tipo € {entero, real}) then W.tipo else tipo_error}
    | AccMenosW         -- {V.tipo = if(W.tipo € {entero, real}) then W.tipo else tipo_error}
    | AccNotW           -- {V.tipo = if(W.tipo = lógico) then tipo_logico else tipo_error}{AUX-=2}
    | AccVDec           -- {V.tipo = if(buscarTipo(id.pos) € {real, entero}) then buscarTipo(id.pos) else tipo_error }
    | AccVW             -- {V.tipo = W.tipo}
    | AccWId            -- {W.tipo = if(O.tipo = void) then buscarTipo(id.pos) else if buscarParametros(id.pos) = O.parametros then buscarTipoRet(id.pos) else tipo_error}
    | AccWTipo          -- {W.tipo = E.tipo}
    | AccWInt           -- {W.tipo = entero}
    | AccWFloat         -- {W.tipo = real}
    | AccWString        -- {W.tipo = cadena}
    | AccWBool           -- {W.tipo = lógico}
    | AccOParam         -- {O.tipo = tipo_funcion, O.parametros = L.parametros, O.numParametros = L.numeroParametros}
    | AccOVoid          -- {O.tipo = void}
    | AccSId            -- {S.tipo = if(bucarTipo(id.pos) = G.tipo = tipo_funcion) then if(buscarParametros(id.pos) != G.parametros) tipo_error else if buscarTipo(id.pos) != G.tipo) then tipo_error}
    | AccSWrite         -- {S.tipo = if (E.tipo !€ {entero, real, cadena) then tipo_error, S.tipoRet = void} 
    | AccSRead          -- {S.tipo = if(buscarTipoTS(id.pos) !€ {entero, real, cadena)) then tipo_error, S.tipoRet = void}
    | AccSReturn        -- {S.tipo = if(X.tipo != S.tipoRet) then tipo_error}
    | AccGAsig          -- {G.tipo = E.tipo}
    | AccGParam         -- {G.tipo = tipo_funcion, G.parametros = L.parametros, G.numParametros = L.numParametros}
    | AccLParam         -- {L.parametros = E.tipo x Q.parametros L.numParametros = 1 + Q.numParametros}
    | AccLVoid          -- {L.parametros = void, L.numParametros=0}
    | AccQParam         -- {Q_1.parametros = E.tipo x Q_2.parametros, Q_1.numParametros = 1 + Q_2.numParametros}
    | AccQVoid          -- {Q.parametros = void, Q.numParametros = 0}
    | AccXTipo          -- {X.tipo = E.tipo}
    | AccXVoid          -- {X.tipo = tipo_void}
    | AccBIf            -- {B.tipo = if(E.tipo != lógico) then tipo_error, Z.tipoRet = B.tipoRet}
    | AccDec5           -- {AUX-=5}
    | AccZS             -- {S.tipoRet = Z.tipoRet}
    | AccDec1           -- {AUX-=1}
    | AccZBloqueRet     -- {C.tipoRet = Z.tipoRet, I.tipoRet = Z.tipoRet}
    | AccDec4           -- {AUX-=4}
    | AccITipoRet       -- {D.tipoRet = I.tipoRet}
    | AccDec2           -- {AUX-=2}
    | AccDIf            -- {D.tipo = if(E.tipo != lógico) then tipo_error, C.tipoRet = D.tipoRet, I.tipoRet = D.tipoRet}
    | AccDec8           -- {AUX-=8}
    | AccDTipoRet       -- {C.tipoRet = D.tipoRet}
    | AccDec3           -- {AUX-=3}
    | AccZonaDecl       -- {zonaDecl=True}
    | AccBLet           -- {añadirTipo,añadirDespl,desplazamiento+=T.tam, zonaDecl = false}
    | AccBTipoRet       -- {S.tipoDevuelto = B.tipoDevuelto}
    | AccTInt           -- {T.tipo = entero, T.tamaño = 2}
    | AccTFloat         -- {T.tipo = real, T.tamaño = 4}
    | AccTBool          -- {T.tipo = logico, T.tamaño = 1}
    | AccTString        -- {T.tipo = cadena, T.tamaño = 64}
    | AccFCrearTabla    -- {añadeTipo(id.pos, tipo_funcion), añadeTipoRet(id.pos, H.tipo), C.tipoRet = H.tipo , añadeEtiq(id.pos, nuevaEtiq())),TSF = crearTabla(), desplazamiento = 0}
    | AccFParamA        -- {zonaDecl = false, añadeParametros(id.pos,A.parametros), añadeNumParametros(id.pos,A.numParametros)}
    | AccFLibTabla      -- {liberarTabla(TSF)} {AUX-=9}
    | AccHTipoT         -- {H.tipo = T.tipo} {AUX-=1}
    | AccHVoid          -- {H.tipo = void}{AUX-=1}
    | AccAIdTipo        -- {id.tipo = T.tipo...}
    | AccAParamK        -- {A.parametros = T.tipo x K.parametros, A.numP = 1 + K.numP}{AUX-=3}
    | AccAVoid          -- {A.numP = 0, A.parametros = NULL}{AUX-=1}
    | AccKParam         -- {K1.parametros = T.tipo x K2.parametros, K1.numP = 1 + K2.numP}{AUX-=4}
    | AccKVoid          -- {k.parametros = NULL, K.numP = 0}
    | AccCTipoRet       -- {B.tipoRet = C_1.tipoRet, C_2.tipoRet = C_1.tipoRet} B C_2}{AUX-=2}
    | AccCrearTabla     -- {TS = crearTabla(), desplazamiento = 0}
    | AccLibTabla       -- {liberarTabla(TS)}{AUX-=1}
    deriving (Show, Eq)

-- Tipos semánticos posibles
data TipoSem
    = TipoEntero
    | TipoReal
    | TipoLogico
    | TipoCadena
    | TipoVoid
    | TipoFuncion
    | TipoError
    deriving (Show, Eq)

-- Información semántica que puede tener un símbolo en la pila auxiliar
data InfoSem = InfoSem
    { tipo           :: TipoSem
    , tamanio        :: Int           -- tamaño en bytes
    , tipoDevuelto   :: TipoSem       -- para funciones
    , parametros     :: [TipoSem]     -- lista de tipos de parámetros
    , numParametros  :: Int
    , posTS          :: Maybe Int     -- posición en tabla de símbolos
    } deriving (Show, Eq)

infoSemInicial :: InfoSem
infoSemInicial = InfoSem
    { tipo          = TipoVoid
    , tamanio       = 0
    , tipoDevuelto  = TipoVoid
    , parametros    = []
    , numParametros = 0
    , posTS         = Nothing
    }

-- El símbolo de la pila combina el símbolo original con su info semántica
data SimboloSem = SimboloSem
    { simbolo :: Simbolo
    , infoSem :: InfoSem
    } deriving (Show)

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
                    
