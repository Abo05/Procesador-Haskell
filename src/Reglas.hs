{- HLINT ignore "Use camelCase" -}
module Reglas where

data NumRegla
    -- Expresiones
    = ReglE_RE1          -- 1:  E  -> R E1
    | ReglE1_AndRE1      -- 2:  E1 -> && R E1
    | ReglE1_Lambda      -- 3:  E1 -> lambda
    | ReglR_UR1          -- 4:  R  -> U R1
    | ReglR1_MayorUR1    -- 5:  R1 -> > U R1
    | ReglR1_MayorIUR1   -- 6:  R1 -> >= U R1
    | ReglR1_Lambda      -- 7:  R1 -> lambda
    | ReglU_VU1          -- 8:  U  -> V U1
    | ReglU1_SumaVU1     -- 9:  U1 -> + V U1
    | ReglU1_RestaVU1    -- 10: U1 -> - V U1
    | ReglU1_Lambda      -- 11: U1 -> lambda
    | ReglV_SumaW        -- 12: V  -> + W
    | ReglV_RestaW       -- 13: V  -> - W
    | ReglV_NotW         -- 14: V  -> ! W
    | ReglV_DecW         -- 15: V  -> -- W
    | ReglV_W            -- 16: V  -> W
    | ReglW_IdO          -- 17: W  -> id O
    | ReglW_ParenE       -- 18: W  -> ( E )
    | ReglW_Entero       -- 19: W  -> entero
    | ReglW_Real         -- 20: W  -> real
    | ReglW_Cadena       -- 21: W  -> cadena
    | ReglW_True         -- 22: W  -> true
    | ReglW_False        -- 23: W  -> false
    | ReglO_ParenL       -- 24: O  -> ( L )
    | ReglO_Lambda       -- 25: O  -> lambda
    -- Sentencias
    | ReglS_IdG          -- 26: S  -> id G ;
    | ReglS_Write        -- 27: S  -> write E ;
    | ReglS_Read         -- 28: S  -> read id ;
    | ReglS_Return       -- 29: S  -> return X ;
    | ReglG_Asig         -- 30: G  -> = E
    | ReglG_Paren        -- 31: G  -> ( L )
    | ReglL_EQ           -- 32: L  -> E Q
    | ReglL_Lambda       -- 33: L  -> lambda
    | ReglQ_ComaEQ       -- 34: Q  -> , E Q
    | ReglQ_Lambda       -- 35: Q  -> lambda
    | ReglX_E            -- 36: X  -> E
    | ReglX_Lambda       -- 37: X  -> lambda
    -- Control de flujo
    | ReglB_If           -- 38: B  -> if ( E ) Z
    | ReglZ_S            -- 39: Z  -> S
    | ReglZ_Bloque       -- 40: Z  -> { C } I
    | ReglI_Else         -- 41: I  -> else D
    | ReglI_Lambda       -- 42: I  -> lambda
    | ReglD_If           -- 43: D  -> if ( E ) { C } I
    | ReglD_Bloque       -- 44: D  -> { C }
    -- Declaraciones
    | ReglB_Let          -- 45: B  -> let T id ;
    | ReglB_S            -- 46: B  -> S
    | ReglT_Int          -- 47: T  -> int
    | ReglT_Float        -- 48: T  -> float
    | ReglT_Boolean      -- 49: T  -> boolean
    | ReglT_String       -- 50: T  -> string
    -- Funciones
    | ReglF_Function     -- 51: F  -> function H id ( A ) { C }
    | ReglH_T            -- 52: H  -> T
    | ReglH_Void         -- 53: H  -> void
    | ReglA_TIdK         -- 54: A  -> T id K
    | ReglA_Void         -- 55: A  -> void
    | ReglK_ComaK        -- 56: K  -> , T id K
    | ReglK_Lambda       -- 57: K  -> lambda
    -- Bloques y programa
    | ReglC_BC           -- 58: C  -> B C
    | ReglC_Lambda       -- 59: C  -> lambda
    | ReglP_BP           -- 60: P  -> B P
    | ReglP_FP           -- 61: P  -> F P
    | ReglP_Lambda       -- 62: P  -> lambda
    | ReglPP_P           -- Nueva regla para el axioma inicial (PP -> P)
    deriving (Show, Eq, Enum)

-- +1 porque Enum empieza en 0
numReglaInt :: NumRegla -> Int
numReglaInt r = fromEnum r + 1  

