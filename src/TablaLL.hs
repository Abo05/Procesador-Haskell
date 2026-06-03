module TablaLL (tablaLL, Regla(..)) where

import Simbolos (Simbolo(..), NoTerminal(..))
import Token (Token(..), mismoTipo)
import Reglas

-- Una regla tiene: número de regla y consecuente 
data Regla = Regla
    { numRegla   :: NumRegla
    , consecuente :: [Simbolo]   -- [] representa lambda
    } deriving (Show)

-- La tabla: dado un no terminal y el token actual, devuelve la regla o Nothin(Error)
-- La notación con $ nos ahorra el uso de paréntesis
tablaLL :: NoTerminal -> Token -> Maybe Regla

--   A 
tablaLL A TkInt     = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0),    NoTerminal K]
tablaLL A TkString  = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkBoolean = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkFloat   = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]
tablaLL A TkVoid    = Just $ Regla ReglA_Void [Terminal TkVoid]

--   B 
tablaLL B (TkIdentificador _) = Just $ Regla ReglB_S [NoTerminal S]
tablaLL B TkRead   = Just $ Regla ReglB_S [NoTerminal S]
tablaLL B TkReturn = Just $ Regla ReglB_S [NoTerminal S]
tablaLL B TkWrite  = Just $ Regla ReglB_S [NoTerminal S]

tablaLL B TkLet    = Just $ Regla ReglB_Let [Terminal TkLet, NoTerminal T,
                                         Terminal (TkIdentificador 0), Terminal TkPuntoComa]
tablaLL B TkIf     = Just $ Regla ReglB_If [Terminal TkIf, Terminal TkParentesisA, NoTerminal E, Terminal TkParentesisC, NoTerminal Z] 

--    C 
tablaLL C (TkIdentificador _) = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C] 
tablaLL C TkLet    = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C] 
tablaLL C TkLlaveC = Just $ Regla ReglC_Lambda [] 
tablaLL C TkIf     = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C] 
tablaLL C TkRead   = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C]
tablaLL C TkReturn = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C]
tablaLL C TkWrite  = Just $ Regla ReglC_BC [NoTerminal B, NoTerminal C]

--    D
tablaLL D TkLlaveA = Just $ Regla ReglD_Bloque [Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC]
tablaLL D TkIf     = Just $ Regla ReglD_If [Terminal TkIf, Terminal TkParentesisA, NoTerminal E,
                                         Terminal TkParentesisC, Terminal TkLlaveA,
                                         NoTerminal C, Terminal TkLlaveC, NoTerminal I]

--    E
tablaLL E tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkTrue              = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkFalse             = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkSuma              = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkResta             = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkNot               = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1]


--    E1
tablaLL E1 TkAnd         = Just $ Regla ReglE1_AndRE1 [Terminal TkAnd, NoTerminal R, NoTerminal E1]
tablaLL E1 TkParentesisC = Just $ Regla ReglE1_Lambda []
tablaLL E1 TkPuntoComa   = Just $ Regla ReglE1_Lambda []
tablaLL E1 TkComa        = Just $ Regla ReglE1_Lambda []
tablaLL E1 TkEof         = Just $ Regla ReglE1_Lambda []

--    F
tablaLL F TkFunction = Just $ Regla ReglF_Function
    [Terminal TkFunction, NoTerminal H, Terminal (TkIdentificador 0),
     Terminal TkParentesisA, NoTerminal A, Terminal TkParentesisC,
     Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC]

--    G
tablaLL G TkParentesisA = Just $ Regla ReglG_Paren
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC]
tablaLL G TkAsignacion  = Just $ Regla ReglG_Asig [Terminal TkAsignacion, NoTerminal E]

--    H
tablaLL H TkInt     = Just $ Regla ReglH_T [NoTerminal T]
tablaLL H TkString  = Just $ Regla ReglH_T [NoTerminal T]
tablaLL H TkBoolean = Just $ Regla ReglH_T [NoTerminal T]
tablaLL H TkFloat   = Just $ Regla ReglH_T [NoTerminal T]
tablaLL H TkVoid    = Just $ Regla ReglH_Void [Terminal TkVoid]

--    I
tablaLL I TkLlaveC   = Just $ Regla ReglI_Lambda []
tablaLL I TkIf       = Just $ Regla ReglI_Lambda []
tablaLL I TkElse     = Just $ Regla ReglI_Else [Terminal TkElse, NoTerminal D]
tablaLL I TkFunction = Just $ Regla ReglI_Lambda []
tablaLL I TkEof      = Just $ Regla ReglI_Lambda []

--    K
tablaLL K TkParentesisC = Just $ Regla ReglK_Lambda []
tablaLL K TkComa        = Just $ Regla ReglK_ComaK
    [Terminal TkComa, NoTerminal T, Terminal (TkIdentificador 0), NoTerminal K]

--    L
tablaLL L tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkTrue              = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkFalse             = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkSuma              = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkResta             = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkNot               = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q]
             
tablaLL L TkParentesisC = Just $ Regla ReglL_Lambda []

--    O
tablaLL O TkParentesisA = Just $ Regla ReglO_ParenL
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC]
tablaLL O tok = case tok of
                    TkSuma          -> Just $ Regla ReglO_Lambda []
                    TkResta         -> Just $ Regla ReglO_Lambda []
                    TkMayor         -> Just $ Regla ReglO_Lambda []
                    TkMayorIgual    -> Just $ Regla ReglO_Lambda []
                    TkPuntoComa     -> Just $ Regla ReglO_Lambda []
                    TkComa          -> Just $ Regla ReglO_Lambda []
                    TkAnd           -> Just $ Regla ReglO_Lambda []
                    TkParentesisC   -> Just $ Regla ReglO_Lambda []
                    _               -> Nothing

--    P
tablaLL P (TkIdentificador _) = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkLet      = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkIf       = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkFunction = Just $ Regla ReglP_FP [NoTerminal F, NoTerminal P]
tablaLL P TkRead     = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkReturn   = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkWrite    = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P]
tablaLL P TkEof      = Just $ Regla ReglP_Lambda []

--    Q
tablaLL Q TkComa        = Just $ Regla ReglQ_ComaEQ [Terminal TkComa, NoTerminal E, NoTerminal Q]
tablaLL Q TkParentesisC = Just $ Regla ReglQ_Lambda []

--    R
tablaLL R tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkTrue              = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkFalse             = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkSuma              = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkResta             = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkNot               = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1]

--    R1
tablaLL R1 TkParentesisC = Just $ Regla ReglR1_Lambda []
tablaLL R1 TkMayor       = Just $ Regla ReglR1_MayorUR1 [Terminal TkMayor,    NoTerminal U, NoTerminal R1]
tablaLL R1 TkMayorIgual  = Just $ Regla ReglR1_MayorIUR1 [Terminal TkMayorIgual, NoTerminal U, NoTerminal R1]
tablaLL R1 TkAnd         = Just $ Regla ReglR1_Lambda []
tablaLL R1 TkPuntoComa   = Just $ Regla ReglR1_Lambda []
tablaLL R1 TkComa        = Just $ Regla ReglR1_Lambda []

--    S
tablaLL S (TkIdentificador _) = Just $ Regla ReglS_IdG
    [Terminal (TkIdentificador 0), NoTerminal G, Terminal TkPuntoComa]
tablaLL S TkRead   = Just $ Regla ReglS_Read
    [Terminal TkRead, Terminal (TkIdentificador 0), Terminal TkPuntoComa]
tablaLL S TkReturn = Just $ Regla ReglS_Return
    [Terminal TkReturn, NoTerminal X, Terminal TkPuntoComa]
tablaLL S TkWrite  = Just $ Regla ReglS_Write
    [Terminal TkWrite, NoTerminal E, Terminal TkPuntoComa]

--    T
tablaLL T TkInt     = Just $ Regla ReglT_Int [Terminal TkInt]
tablaLL T TkString  = Just $ Regla ReglT_String [Terminal TkString]
tablaLL T TkBoolean = Just $ Regla ReglT_Boolean [Terminal TkBoolean]
tablaLL T TkFloat   = Just $ Regla ReglT_Float [Terminal TkFloat]

--    U
tablaLL U tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkTrue              = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkFalse             = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkSuma              = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkResta             = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkNot               = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1]

--    U1
tablaLL U1 TkParentesisC = Just $ Regla ReglU1_Lambda []
tablaLL U1 TkSuma        = Just $ Regla ReglU1_SumaVU1  [Terminal TkSuma,  NoTerminal V, NoTerminal U1]
tablaLL U1 TkResta       = Just $ Regla ReglU1_RestaVU1 [Terminal TkResta, NoTerminal V, NoTerminal U1]
tablaLL U1 TkMayor       = Just $ Regla ReglU1_Lambda []
tablaLL U1 TkMayorIgual  = Just $ Regla ReglU1_Lambda []
tablaLL U1 TkAnd         = Just $ Regla ReglU1_Lambda []
tablaLL U1 TkPuntoComa   = Just $ Regla ReglU1_Lambda []
tablaLL U1 TkComa        = Just $ Regla ReglU1_Lambda []

--    V
tablaLL V tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok TkTrue              = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok TkFalse             = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglV_W [NoTerminal W]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglV_W [NoTerminal W]

tablaLL V TkSuma            = Just $ Regla ReglV_SumaW [Terminal TkSuma,  NoTerminal W]
tablaLL V TkResta           = Just $ Regla ReglV_RestaW [Terminal TkResta, NoTerminal W]
tablaLL V TkNot             = Just $ Regla ReglV_NotW [Terminal TkNot,   NoTerminal W]
tablaLL V TkAutodecremento  = Just $ Regla ReglV_DecW
    [Terminal TkAutodecremento, Terminal (TkIdentificador 0)]

--    W
tablaLL W (TkEntero _)       = Just $ Regla ReglW_Entero [Terminal (TkEntero 0)]
tablaLL W (TkReal _)         = Just $ Regla ReglW_Real [Terminal (TkReal 0)]
tablaLL W (TkCadena _)       = Just $ Regla ReglW_Cadena [Terminal (TkCadena "")]
tablaLL W TkTrue             = Just $ Regla ReglW_True [Terminal TkTrue]
tablaLL W TkFalse            = Just $ Regla ReglW_False [Terminal TkFalse]
tablaLL W TkParentesisA      = Just $ Regla ReglW_ParenE
    [Terminal TkParentesisA, NoTerminal E, Terminal TkParentesisC]
tablaLL W (TkIdentificador _)= Just $ Regla ReglW_IdO
    [Terminal (TkIdentificador 0), NoTerminal O]

--    X
tablaLL X tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkTrue              = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkFalse             = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkSuma              = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkResta             = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkNot               = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglX_E [NoTerminal E]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglX_E [NoTerminal E]

tablaLL X TkPuntoComa = Just $ Regla ReglX_Lambda []

--    Z
tablaLL Z TkLlaveA          = Just $ Regla ReglZ_Bloque
    [Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC, NoTerminal I]
tablaLL Z (TkIdentificador _) = Just $ Regla ReglZ_S [NoTerminal S]
tablaLL Z TkRead   = Just $ Regla ReglZ_S [NoTerminal S]
tablaLL Z TkReturn = Just $ Regla ReglZ_S [NoTerminal S]
tablaLL Z TkWrite  = Just $ Regla ReglZ_S [NoTerminal S]

-- Caso por defecto: celda vacía en la tabla
tablaLL _ _ = Nothing
