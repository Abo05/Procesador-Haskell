module TablaLL (tablaLL, Regla(..)) where

import Simbolos (Simbolo(..), NoTerminal(..), Accion(..))
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
tablaLL A TkInt     = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), Accion AccAIdTipo, NoTerminal K, Accion AccAParamK]
tablaLL A TkString  = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), Accion AccAIdTipo, NoTerminal K, Accion AccAParamK]
tablaLL A TkBoolean = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), Accion AccAIdTipo, NoTerminal K, Accion AccAParamK]
tablaLL A TkFloat   = Just $ Regla ReglA_TIdK [NoTerminal T, Terminal (TkIdentificador 0), Accion AccAIdTipo, NoTerminal K, Accion AccAParamK]
tablaLL A TkVoid    = Just $ Regla ReglA_Void [Terminal TkVoid, Accion AccAVoid]

--   B 
tablaLL B (TkIdentificador _) = Just $ Regla ReglB_S [Accion AccBTipoRet, NoTerminal S, Accion AccDec1]
tablaLL B TkRead   = Just $ Regla ReglB_S [Accion AccBTipoRet, NoTerminal S, Accion AccDec1]
tablaLL B TkReturn = Just $ Regla ReglB_S [Accion AccBTipoRet, NoTerminal S, Accion AccDec1]
tablaLL B TkWrite  = Just $ Regla ReglB_S [Accion AccBTipoRet, NoTerminal S, Accion AccDec1]

tablaLL B TkLet    = Just $ Regla ReglB_Let [Terminal TkLet, Accion AccZonaDecl, NoTerminal T,
                                         Terminal (TkIdentificador 0), Terminal TkPuntoComa, Accion AccBLet]
tablaLL B TkIf     = Just $ Regla ReglB_If [Terminal TkIf, Terminal TkParentesisA, NoTerminal E, Terminal TkParentesisC, Accion AccBIf, NoTerminal Z, Accion AccDec5] 

--    C 
tablaLL C (TkIdentificador _) = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2] 
tablaLL C TkLet    = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2] 
tablaLL C TkLlaveC = Just $ Regla ReglC_Lambda [] 
tablaLL C TkIf     = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2] 
tablaLL C TkRead   = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2]
tablaLL C TkReturn = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2]
tablaLL C TkWrite  = Just $ Regla ReglC_BC [Accion AccCTipoRet, NoTerminal B, NoTerminal C, Accion AccDec2]

--    D
tablaLL D TkLlaveA = Just $ Regla ReglD_Bloque [Accion AccDTipoRet, Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC, Accion AccDec3]
tablaLL D TkIf     = Just $ Regla ReglD_If [Terminal TkIf, Terminal TkParentesisA, NoTerminal E,
                                         Terminal TkParentesisC, Accion AccDIf, Terminal TkLlaveA,
                                         NoTerminal C, Terminal TkLlaveC, NoTerminal I, Accion AccDec8]

--    E
tablaLL E tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkTrue              = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkFalse             = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkSuma              = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkResta             = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkNot               = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglE_RE1 [NoTerminal R, NoTerminal E1, Accion AccER]


--    E1
tablaLL E1 TkAnd         = Just $ Regla ReglE1_AndRE1 [Terminal TkAnd, NoTerminal R, NoTerminal E1, Accion AccE1And]
tablaLL E1 TkParentesisC = Just $ Regla ReglE1_Lambda [Accion AccE1Void]
tablaLL E1 TkPuntoComa   = Just $ Regla ReglE1_Lambda [Accion AccE1Void]
tablaLL E1 TkComa        = Just $ Regla ReglE1_Lambda [Accion AccE1Void]
tablaLL E1 TkEof         = Just $ Regla ReglE1_Lambda [Accion AccE1Void]

--    F
tablaLL F TkFunction = Just $ Regla ReglF_Function
    [Terminal TkFunction, Accion AccZonaDecl, NoTerminal H, Terminal (TkIdentificador 0),
     Accion AccFCrearTabla, Terminal TkParentesisA, NoTerminal A, Accion AccFParamA, Terminal TkParentesisC,
     Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC, Accion AccFLibTabla]

--    G
tablaLL G TkParentesisA = Just $ Regla ReglG_Paren
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC, Accion AccGParam]
tablaLL G TkAsignacion  = Just $ Regla ReglG_Asig [Terminal TkAsignacion, NoTerminal E, Accion AccGAsig]

--    H
tablaLL H TkInt     = Just $ Regla ReglH_T [NoTerminal T, Accion AccHTipoT]
tablaLL H TkString  = Just $ Regla ReglH_T [NoTerminal T, Accion AccHTipoT]
tablaLL H TkBoolean = Just $ Regla ReglH_T [NoTerminal T, Accion AccHTipoT]
tablaLL H TkFloat   = Just $ Regla ReglH_T [NoTerminal T, Accion AccHTipoT]
tablaLL H TkVoid    = Just $ Regla ReglH_Void [Terminal TkVoid, Accion AccHVoid]

--    I
tablaLL I TkLlaveC   = Just $ Regla ReglI_Lambda []
tablaLL I TkIf       = Just $ Regla ReglI_Lambda []
tablaLL I TkElse     = Just $ Regla ReglI_Else [Terminal TkElse, Accion AccITipoRet, NoTerminal D, Accion AccDec2]
tablaLL I TkFunction = Just $ Regla ReglI_Lambda []
tablaLL I TkEof      = Just $ Regla ReglI_Lambda []

--    K
tablaLL K TkParentesisC = Just $ Regla ReglK_Lambda [Accion AccKVoid]
tablaLL K TkComa        = Just $ Regla ReglK_ComaK
    [Terminal TkComa, NoTerminal T, Terminal (TkIdentificador 0), Accion AccAIdTipo, NoTerminal K, Accion AccKParam]

--    L
tablaLL L tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkTrue              = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkFalse             = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkSuma              = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkResta             = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkNot               = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglL_EQ [NoTerminal E, NoTerminal Q, Accion AccLParam]
             
tablaLL L TkParentesisC = Just $ Regla ReglL_Lambda [Accion AccLVoid]

--    O
tablaLL O TkParentesisA = Just $ Regla ReglO_ParenL
    [Terminal TkParentesisA, NoTerminal L, Terminal TkParentesisC, Accion AccOParam]
tablaLL O tok = case tok of
                    TkSuma          -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkResta         -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkMayor         -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkMayorIgual    -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkPuntoComa     -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkComa          -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkAnd           -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    TkParentesisC   -> Just $ Regla ReglO_Lambda [Accion AccOVoid]
                    _               -> Nothing

--    P
tablaLL P (TkIdentificador _) = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkLet      = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkIf       = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkFunction = Just $ Regla ReglP_FP [NoTerminal F, NoTerminal P, Accion AccDec2]
tablaLL P TkRead     = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkReturn   = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkWrite    = Just $ Regla ReglP_BP [NoTerminal B, NoTerminal P, Accion AccDec2]
tablaLL P TkEof      = Just $ Regla ReglP_Lambda []

--    PP
tablaLL PP (TkIdentificador _) = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkLet      = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkIf       = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkFunction = Just $ Regla ReglP_FP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkRead     = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkReturn   = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkWrite    = Just $ Regla ReglP_BP [Accion AccCrearTabla, NoTerminal P, Accion AccLibTabla]
tablaLL PP TkEof      = Just $ Regla ReglP_Lambda []

--    Q
tablaLL Q TkComa        = Just $ Regla ReglQ_ComaEQ [Terminal TkComa, NoTerminal E, NoTerminal Q, Accion AccQParam]
tablaLL Q TkParentesisC = Just $ Regla ReglQ_Lambda [Accion AccQVoid]

--    R
tablaLL R tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkTrue              = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkFalse             = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkSuma              = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkResta             = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkNot               = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglR_UR1 [NoTerminal U, NoTerminal R1, Accion AccRU]

--    R1
tablaLL R1 TkParentesisC = Just $ Regla ReglR1_Lambda [Accion AccR1Void]
tablaLL R1 TkMayor       = Just $ Regla ReglR1_MayorUR1 [Terminal TkMayor,    NoTerminal U, NoTerminal R1, Accion AccR1M]
tablaLL R1 TkMayorIgual  = Just $ Regla ReglR1_MayorIUR1 [Terminal TkMayorIgual, NoTerminal U, NoTerminal R1, Accion AccR1MI]
tablaLL R1 TkAnd         = Just $ Regla ReglR1_Lambda [Accion AccR1Void]
tablaLL R1 TkPuntoComa   = Just $ Regla ReglR1_Lambda [Accion AccR1Void]
tablaLL R1 TkComa        = Just $ Regla ReglR1_Lambda [Accion AccR1Void]

--    S
tablaLL S (TkIdentificador _) = Just $ Regla ReglS_IdG
    [Terminal (TkIdentificador 0), NoTerminal G, Terminal TkPuntoComa, Accion AccSId]
tablaLL S TkRead   = Just $ Regla ReglS_Read
    [Terminal TkRead, Terminal (TkIdentificador 0), Terminal TkPuntoComa, Accion AccSRead]
tablaLL S TkReturn = Just $ Regla ReglS_Return
    [Terminal TkReturn, NoTerminal X, Terminal TkPuntoComa, Accion AccSReturn]
tablaLL S TkWrite  = Just $ Regla ReglS_Write
    [Terminal TkWrite, NoTerminal E, Terminal TkPuntoComa, Accion AccSWrite]

--    T
tablaLL T TkInt     = Just $ Regla ReglT_Int [Terminal TkInt, Accion AccTInt]
tablaLL T TkString  = Just $ Regla ReglT_String [Terminal TkString, Accion AccTString]
tablaLL T TkBoolean = Just $ Regla ReglT_Boolean [Terminal TkBoolean, Accion AccTBool]
tablaLL T TkFloat   = Just $ Regla ReglT_Float [Terminal TkFloat, Accion AccTFloat]

--    U
tablaLL U tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkTrue              = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkFalse             = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkSuma              = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkResta             = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkNot               = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglU_VU1 [NoTerminal V, NoTerminal U1, Accion AccUV]

--    U1
tablaLL U1 TkParentesisC = Just $ Regla ReglU1_Lambda [Accion AccU1Void]
tablaLL U1 TkSuma        = Just $ Regla ReglU1_SumaVU1  [Terminal TkSuma,  NoTerminal V, NoTerminal U1, Accion AccU1Suma]
tablaLL U1 TkResta       = Just $ Regla ReglU1_RestaVU1 [Terminal TkResta, NoTerminal V, NoTerminal U1, Accion AccU1Resta]
tablaLL U1 TkMayor       = Just $ Regla ReglU1_Lambda [Accion AccU1Void]
tablaLL U1 TkMayorIgual  = Just $ Regla ReglU1_Lambda [Accion AccU1Void]
tablaLL U1 TkAnd         = Just $ Regla ReglU1_Lambda [Accion AccU1Void]
tablaLL U1 TkPuntoComa   = Just $ Regla ReglU1_Lambda [Accion AccU1Void]
tablaLL U1 TkComa        = Just $ Regla ReglU1_Lambda [Accion AccU1Void]

--    V
tablaLL V tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok TkTrue              = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok TkFalse             = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglV_W [NoTerminal W, Accion AccVW]

tablaLL V TkSuma            = Just $ Regla ReglV_SumaW [Terminal TkSuma,  NoTerminal W, Accion AccMasW]
tablaLL V TkResta           = Just $ Regla ReglV_RestaW [Terminal TkResta, NoTerminal W, Accion AccMenosW]
tablaLL V TkNot             = Just $ Regla ReglV_NotW [Terminal TkNot,   NoTerminal W, Accion AccNotW]
tablaLL V TkAutodecremento  = Just $ Regla ReglV_DecW
    [Terminal TkAutodecremento, Terminal (TkIdentificador 0), Accion AccVDec]

--    W
tablaLL W (TkEntero _)       = Just $ Regla ReglW_Entero [Terminal (TkEntero 0), Accion AccWInt]
tablaLL W (TkReal _)         = Just $ Regla ReglW_Real [Terminal (TkReal 0), Accion AccWFloat]
tablaLL W (TkCadena _)       = Just $ Regla ReglW_Cadena [Terminal (TkCadena ""), Accion AccWString]
tablaLL W TkTrue             = Just $ Regla ReglW_True [Terminal TkTrue, Accion AccWBool]
tablaLL W TkFalse            = Just $ Regla ReglW_False [Terminal TkFalse, Accion AccWBool]
tablaLL W TkParentesisA      = Just $ Regla ReglW_ParenE
    [Terminal TkParentesisA, NoTerminal E, Terminal TkParentesisC, Accion AccWTipo]
tablaLL W (TkIdentificador _)= Just $ Regla ReglW_IdO
    [Terminal (TkIdentificador 0), NoTerminal O, Accion AccWId]

--    X
tablaLL X tok 
            | mismoTipo tok (TkEntero 0)        = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok (TkReal 0)          = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok (TkCadena "")       = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkTrue              = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkFalse             = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkParentesisA       = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkSuma              = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkResta             = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkNot               = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok TkAutodecremento    = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]
            | mismoTipo tok (TkIdentificador 0) = Just $ Regla ReglX_E [NoTerminal E, Accion AccXTipo]

tablaLL X TkPuntoComa = Just $ Regla ReglX_Lambda [Accion AccXVoid]

--    Z
tablaLL Z TkLlaveA          = Just $ Regla ReglZ_Bloque
    [Accion AccZBloqueRet, Terminal TkLlaveA, NoTerminal C, Terminal TkLlaveC, NoTerminal I, Accion AccDec4]
tablaLL Z (TkIdentificador _) = Just $ Regla ReglZ_S [Accion AccZS, NoTerminal S, Accion AccDec1]
tablaLL Z TkRead   = Just $ Regla ReglZ_S [Accion AccZS, NoTerminal S, Accion AccDec1]
tablaLL Z TkReturn = Just $ Regla ReglZ_S [Accion AccZS, NoTerminal S, Accion AccDec1]
tablaLL Z TkWrite  = Just $ Regla ReglZ_S [Accion AccZS, NoTerminal S, Accion AccDec1]

-- Caso por defecto: celda vacía en la tabla
tablaLL _ _ = Nothing
