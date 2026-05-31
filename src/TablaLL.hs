module TablaLL (tablaLL, Regla(..)) where

import Simbolos (Simbolo(..), NoTerminal(..))
import Token (Token(..), mismoTipo)

-- Una regla tiene: número de regla y consecuente 
data Regla = Regla
    { numRegla   :: Int
    , consecuente :: [Simbolo]   -- [] representa lambda
    } deriving (Show)

-- La tabla: dado un no terminal y el token actual, devuelve la regla o Nothin(Error)
tablaLL :: NoTerminal -> Token -> Maybe Regla

--   A 
tablaLL A TkInt     = Just $ Regla 54 [NoTerminal T, Terminal (TkIdentificador 0),    NoTerminal K]
tablaLL A TkString  = Just $ Regla 54 [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkBoolean = Just $ Regla 54 [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkFloat   = Just $ Regla 54 [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkVoid    = Just $ Regla 55 [Terminal TkVoid]

--   B 
tablaLL B (TkIdentificador _) = Just $ Regla 46 [NoTerminal S]
tablaLL B TkRead   = Just $ Regla 46 [NoTerminal S]
tablaLL B TkReturn = Just $ Regla 46 [NoTerminal S]
tablaLL B TkWrite  = Just $ Regla 46 [NoTerminal S]

tablaLL B TkLet    = Just $ Regla 45 [Terminal TkLet, NoTerminal T,
                                         Terminal (TkIdentificador 0), Terminal TkPuntoComa]
tablaLL B TkIf     = Just $ Regla 38 [Terminal TkIf, Terminal TkParentesisA,
                                         NoTerminal E, Terminal TkParentesisC, NoTerminal Z]

--    C
tablaLL C (TkIdentificador _) = Just $ Regla 58 [NoTerminal B, NoTerminal C]
tablaLL C TkLet    = Just $ Regla 58 [NoTerminal B, NoTerminal C]
tablaLL C TkLlaveC = Just $ Regla 59 []
tablaLL C TkIf     = Just $ Regla 58 [NoTerminal B, NoTerminal C]
tablaLL C TkRead   = Just $ Regla 58 [NoTerminal B, NoTerminal C]
tablaLL C TkReturn = Just $ Regla 58 [NoTerminal B, NoTerminal C]
tablaLL C TkWrite  = Just $ Regla 58 [NoTerminal B, NoTerminal C]

--    D
tablaLL D TkLlaveA = Just $ Regla 44 [Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC]
tablaLL D TkIf     = Just $ Regla 43 [Terminal TkIf, Terminal TkParentesisA, NoTerminal E,
                                         Terminal TkParentesisC, Terminal TkLlaveA,
                                         NoTerminal C, Terminal TkLlaveC, NoTerminal I]

--    E
tablaLL E tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkTrue              = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkFalse             = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkParentesisA       = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkSuma              = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkResta             = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkNot               = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla 1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 1 [NoTerminal R, NoTerminal E1]


--    E1
tablaLL E1 TkAnd         = Just $ Regla 2 [Terminal TkAnd, NoTerminal R, NoTerminal E1]
tablaLL E1 TkParentesisC = Just $ Regla 3 []
tablaLL E1 TkPuntoComa   = Just $ Regla 3 []
tablaLL E1 TkComa        = Just $ Regla 3 []
tablaLL E1 TkEof         = Just $ Regla 3 []

--    F
tablaLL F TkFunction = Just $ Regla 51
    [Terminal TkFunction, NoTerminal H, Terminal (TkIdentificador 0),
     Terminal TkParentesisA, NoTerminal A, Terminal TkParentesisC,
     Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC]

--    G
tablaLL G TkParentesisA = Just $ Regla 31
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC]
tablaLL G TkAsignacion  = Just $ Regla 30 [Terminal TkAsignacion, NoTerminal E]

--    H
tablaLL H TkInt     = Just $ Regla 52 [NoTerminal T]
tablaLL H TkString  = Just $ Regla 52 [NoTerminal T]
tablaLL H TkBoolean = Just $ Regla 52 [NoTerminal T]
tablaLL H TkFloat   = Just $ Regla 52 [NoTerminal T]
tablaLL H TkVoid    = Just $ Regla 53 [Terminal TkVoid]

--    I
tablaLL I TkLlaveC   = Just $ Regla 42 []
tablaLL I TkIf       = Just $ Regla 42 []
tablaLL I TkElse     = Just $ Regla 41 [Terminal TkElse, NoTerminal D]
tablaLL I TkFunction = Just $ Regla 42 []
tablaLL I TkEof      = Just $ Regla 42 []

--    K
tablaLL K TkParentesisC = Just $ Regla 57 []
tablaLL K TkComa        = Just $ Regla 56
    [Terminal TkComa, NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]

--    L
tablaLL L tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkTrue              = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkFalse             = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkParentesisA       = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkSuma              = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkResta             = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkNot               = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkAutodecremento    = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 32 [NoTerminal E, NoTerminal Q]
             
tablaLL L TkParentesisC = Just $ Regla 33 []

--    O
tablaLL O TkParentesisA = Just $ Regla 24
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC]
tablaLL O tok = case tok of
                    TkSuma          -> Just $ Regla 25 []
                    TkResta         -> Just $ Regla 25 []
                    TkMayor         -> Just $ Regla 25 []
                    TkMayorIgual    -> Just $ Regla 25 []
                    TkPuntoComa     -> Just $ Regla 25 []
                    TkComa          -> Just $ Regla 25 []
                    TkAnd           -> Just $ Regla 25 []
                    TkParentesisC   -> Just $ Regla 25 []
                    _               -> Nothing

--    P
tablaLL P (TkIdentificador _) = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkLet      = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkIf       = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkFunction = Just $ Regla 61 [NoTerminal F, NoTerminal P]
tablaLL P TkRead     = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkReturn   = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkWrite    = Just $ Regla 60 [NoTerminal B, NoTerminal P]
tablaLL P TkEof      = Just $ Regla 62 []

--    Q
tablaLL Q TkComa        = Just $ Regla 34 [Terminal TkComa, NoTerminal E, NoTerminal Q]
tablaLL Q TkParentesisC = Just $ Regla 35 []

--    R
tablaLL R tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkTrue              = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkFalse             = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkParentesisA       = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkSuma              = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkResta             = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkNot               = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla 4 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 4 [NoTerminal U, NoTerminal R1]

--    R1
tablaLL R1 TkParentesisC = Just $ Regla 7 []
tablaLL R1 TkMayor       = Just $ Regla 5 [Terminal TkMayor,    NoTerminal U, NoTerminal R1]
tablaLL R1 TkMayorIgual  = Just $ Regla 6 [Terminal TkMayorIgual, NoTerminal U, NoTerminal R1]
tablaLL R1 TkAnd         = Just $ Regla 7 []
tablaLL R1 TkPuntoComa   = Just $ Regla 7 []
tablaLL R1 TkComa        = Just $ Regla 7 []

--    S
tablaLL S (TkIdentificador _) = Just $ Regla 26
    [Terminal (TkIdentificador 0), NoTerminal G, Terminal TkPuntoComa]
tablaLL S TkRead   = Just $ Regla 28
    [Terminal TkRead, Terminal (TkIdentificador 0), Terminal TkPuntoComa]
tablaLL S TkReturn = Just $ Regla 29
    [Terminal TkReturn, NoTerminal X, Terminal TkPuntoComa]
tablaLL S TkWrite  = Just $ Regla 27
    [Terminal TkWrite, NoTerminal E, Terminal TkPuntoComa]

--    T
tablaLL T TkInt     = Just $ Regla 47 [Terminal TkInt]
tablaLL T TkString  = Just $ Regla 50 [Terminal TkString]
tablaLL T TkBoolean = Just $ Regla 49 [Terminal TkBoolean]
tablaLL T TkFloat   = Just $ Regla 48 [Terminal TkFloat]

--    U
tablaLL U tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkTrue              = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkFalse             = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkParentesisA       = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkSuma              = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkResta             = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkNot               = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla 8 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 8 [NoTerminal V, NoTerminal U1]

--    U1
tablaLL U1 TkParentesisC = Just $ Regla 11 []
tablaLL U1 TkSuma        = Just $ Regla 9  [Terminal TkSuma,  NoTerminal V, NoTerminal U1]
tablaLL U1 TkResta       = Just $ Regla 10 [Terminal TkResta, NoTerminal V, NoTerminal U1]
tablaLL U1 TkMayor       = Just $ Regla 11 []
tablaLL U1 TkMayorIgual  = Just $ Regla 11 []
tablaLL U1 TkAnd         = Just $ Regla 11 []
tablaLL U1 TkPuntoComa   = Just $ Regla 11 []
tablaLL U1 TkComa        = Just $ Regla 11 []

--    V
tablaLL V tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok TkTrue              = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok TkFalse             = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok TkParentesisA       = Just $ Regla 16 [NoTerminal W]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 16 [NoTerminal W]

tablaLL V TkSuma            = Just $ Regla 12 [Terminal TkSuma,  NoTerminal W]
tablaLL V TkResta           = Just $ Regla 13 [Terminal TkResta, NoTerminal W]
tablaLL V TkNot             = Just $ Regla 14 [Terminal TkNot,   NoTerminal W]
tablaLL V TkAutodecremento  = Just $ Regla 15
    [Terminal TkAutodecremento, Terminal (TkIdentificador 0)]

--    W
tablaLL W (TkEntero _)       = Just $ Regla 19 [Terminal (TkEntero 0)]
tablaLL W (TkReal _)         = Just $ Regla 20 [Terminal (TkReal 0)]
tablaLL W (TkCadena _)       = Just $ Regla 21 [Terminal (TkCadena "")]
tablaLL W TkTrue             = Just $ Regla 22 [Terminal TkTrue]
tablaLL W TkFalse            = Just $ Regla 23 [Terminal TkFalse]
tablaLL W TkParentesisA      = Just $ Regla 18
    [Terminal TkParentesisA, NoTerminal E, Terminal TkParentesisC]
tablaLL W (TkIdentificador _)= Just $ Regla 17
    [Terminal (TkIdentificador 0), NoTerminal O]

--    X
tablaLL X tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok (TkReal 0)          = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok (TkCadena "")       = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkTrue              = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkFalse             = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkParentesisA       = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkSuma              = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkResta             = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkNot               = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok TkAutodecremento    = Just $ Regla 36 [NoTerminal E]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla 36 [NoTerminal E]

tablaLL X TkPuntoComa = Just $ Regla 37 []

--    Z
tablaLL Z TkLlaveA          = Just $ Regla 40
    [Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC, NoTerminal I]
tablaLL Z (TkIdentificador _) = Just $ Regla 39 [NoTerminal S]
tablaLL Z TkRead   = Just $ Regla 39 [NoTerminal S]
tablaLL Z TkReturn = Just $ Regla 39 [NoTerminal S]
tablaLL Z TkWrite  = Just $ Regla 39 [NoTerminal S]

-- Caso por defecto: celda vacía en la tabla
tablaLL _ _ = Nothing
