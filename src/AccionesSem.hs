module AccionesSem (aplicarAccion) where

import Simbolos      (SimboloSem(..), InfoSem(..), Accion(..), TipoSem(..), tipoAString, mostrarParametros)
import GErrores      (GError, registrarErrorAsin, CodErr(..))
import TablaSimbolos (TablaSimbolos, liberarTablaLocal, getInfo, zonaDecl, asignarTipoYDespl, updateInfo, crearTablaLocal)

-- Acceso a la pila auxiliar por índice desde la cima (0 = cima)
getAux :: [SimboloSem] -> Int -> InfoSem
getAux aux i = infoSem (aux !! i)

-- Modificar un elemento de la pila auxiliar por índice desde la cima
setAux :: [SimboloSem] -> Int -> (InfoSem -> InfoSem) -> [SimboloSem]
setAux aux i f =
    let (antes, sim:despues) = splitAt i aux
    in antes ++ [sim { infoSem = f (infoSem sim) }] ++ despues

-- Liberar n símbolos de la cima de la pila auxiliar
liberarSimbolos :: [SimboloSem] -> Int -> [SimboloSem]
liberarSimbolos aux n = drop n aux

-- Registrar un error semántico con código ErrTipos
errorSem :: String -> GError -> GError
errorSem msg ge = registrarErrorAsin ErrSemantico msg ge

aplicarAccion :: Accion
              -> GError -> TablaSimbolos
              -> [SimboloSem]   -- pila principal (para heredados)
              -> [SimboloSem]   -- pila auxiliar  (para sintetizados)
              -> IO (GError, TablaSimbolos, [SimboloSem], [SimboloSem])

-- Regla 1: AccER
aplicarAccion AccER ge ts pila aux =
    let e1Tipo = tipo (getAux aux 0)
        rTipo  = tipo (getAux aux 1)
        tRes   = if e1Tipo == TipoVoid || rTipo == e1Tipo
                    then rTipo
                    else TipoError
        msg    = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
               ++ tipoAString rTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString e1Tipo
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 2: AccE1And
aplicarAccion AccE1And ge ts pila aux =
    let rTipo  = tipo (getAux aux 1)
        e1_1   = tipo (getAux aux 0)
        ge1    = if rTipo /= TipoLogico
                    then errorSem
                        ("Se esperaba obtener una expresión de tipo lógico antes del &&, sin embargo se obtuvo una expresión tipo "
                         ++ tipoAString rTipo) ge
                    else ge
        tRes   = if rTipo == TipoLogico && (e1_1 == TipoLogico || e1_1 == TipoVoid)
                    then TipoLogico
                    else TipoError
        ge2    = if tRes == TipoError && rTipo == TipoLogico
                    then errorSem
                        ("Se esperaba obtener una expresión de tipo lógico después del &&, sin embargo se obtuvo una expresión tipo "
                         ++ tipoAString e1_1) ge1
                    else ge1
        aux'   = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge2, ts, pila, aux')

-- Regla 3: AccE1Void
aplicarAccion AccE1Void ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Regla 4: AccRU
aplicarAccion AccRU ge ts pila aux =
    let r1Tipo = tipo (getAux aux 0)
        uTipo  = tipo (getAux aux 1)
        tRes   = if r1Tipo == TipoVoid
                    then uTipo
                    else if uTipo == r1Tipo then uTipo else TipoError
        msg    = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
               ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1Tipo
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 5: AccR1M  (operador >)
aplicarAccion AccR1M ge ts pila aux =
    let r1_2Tipo = tipo (getAux aux 0)
        uTipo    = tipo (getAux aux 1)
        tRes     = if (uTipo == TipoEntero || uTipo == TipoReal) && (r1_2Tipo == TipoVoid || r1_2Tipo == uTipo)
                    then uTipo else TipoError
        msg      = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1_2Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 6: AccR1MI  (operador >=)
aplicarAccion AccR1MI ge ts pila aux =
    let r1_2Tipo = tipo (getAux aux 0)
        uTipo    = tipo (getAux aux 1)
        tRes     = if (uTipo == TipoEntero || uTipo == TipoReal) && (r1_2Tipo == TipoVoid || r1_2Tipo == uTipo)
                    then uTipo else TipoError
        msg      = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1_2Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 7: AccR1Void
aplicarAccion AccR1Void ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Regla 8: AccUV
aplicarAccion AccUV ge ts pila aux =
    let u1Tipo = tipo (getAux aux 0)
        vTipo = tipo (getAux aux 1)
        tRes     = if (u1Tipo == TipoVoid || vTipo == u1Tipo)
                    then vTipo else TipoError
        msg      = "Se están operando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 9: AccU1Suma
aplicarAccion AccU1Suma ge ts pila aux =
    let u1Tipo = tipo (getAux aux 0)
        vTipo = tipo (getAux aux 1)
        tRes     = if (vTipo == TipoEntero || vTipo == TipoReal) && (u1Tipo == TipoVoid || u1Tipo == vTipo)
                    then vTipo else TipoError
        msg
          | vTipo /= TipoEntero && vTipo /= TipoReal =
              "Solo se pueden sumar expresiones del tipo entero o real. La primera expresión es del tipo "
              ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
          | otherwise =
              "Se está sumando dos expresiones de distinto tipo, la primera expresión es del tipo "
              ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 10: AccU1Suma
aplicarAccion AccU1Resta ge ts pila aux =
    let u1Tipo = tipo (getAux aux 0)
        vTipo = tipo (getAux aux 1)
        tRes     = if (vTipo == TipoEntero || vTipo == TipoReal) && (u1Tipo == TipoVoid || u1Tipo == vTipo)
                    then vTipo else TipoError
        msg
          | vTipo /= TipoEntero && vTipo /= TipoReal =
              "Solo se pueden restar expresiones del tipo entero o real. La primera expresión es del tipo "
              ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
          | otherwise =
              "Se está restando dos expresiones de distinto tipo, la primera expresión es del tipo "
              ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 11: AccU1Void
aplicarAccion AccU1Void ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Regla 12: AccMasW
aplicarAccion AccMasW ge ts pila aux =
    let wTipo = tipo (getAux aux 0)
        tRes     = if wTipo == TipoEntero || wTipo == TipoReal
                    then wTipo else TipoError
        msg      = "La suma unaria solo se puede utilizar con tipos enteros o reales, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 13: AccMenosW
aplicarAccion AccMenosW ge ts pila aux =
    let wTipo = tipo (getAux aux 0)
        tRes     = if wTipo == TipoEntero || wTipo == TipoReal
                    then wTipo else TipoError
        msg      = "La resta unaria solo se puede utilizar con tipos enteros o reales, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 14: AccNotW
aplicarAccion AccNotW ge ts pila aux =
    let wTipo = tipo (getAux aux 0)
        tRes     = if wTipo == TipoLogico
                    then wTipo else TipoError
        msg      = "La operación de negación solo se puede utilizar con tipos lógicos, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 15: AccVDec
aplicarAccion AccVDec ge ts pila aux =
    let posId  = posTS (getAux aux 2)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        oTipo  = tipo (getAux aux 0)
        tRes   = if oTipo == TipoVoid
                    then idTipo
                    else if parametros (getAux aux 0) == maybe [] parametros idInfo
                            then maybe TipoError tipoRet idInfo
                            else TipoError
        msg    = "Los parametros de la llamada a la función son incorrectos"
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
    
-- Regla 16: AccVW
aplicarAccion AccVW ge ts pila aux =
    let tRes  = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 17: AccWId
aplicarAccion AccWId ge ts pila aux =
    let posId  = posTS (getAux aux 2)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        oTipo  = tipo (getAux aux 0)
        tRes   = if oTipo == TipoVoid
                    then idTipo
                    else if parametros (getAux aux 0) == maybe [] parametros idInfo
                            then maybe TipoError tipoRet idInfo
                            else TipoError
        msg    = "Los parametros de la llamada a la función son incorrectos"
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Regla 18: AccWTipo
aplicarAccion AccWTipo ge ts pila aux =
    let tRes  = tipo (getAux aux 1)
        aux'    = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 19: AccWInt
aplicarAccion AccWInt ge ts pila aux =
    let tRes  = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 20: AccWFloat
aplicarAccion AccWFloat ge ts pila aux =
    let tRes  = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 21: AccWString
aplicarAccion AccWString ge ts pila aux =
    let tRes  = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 22: AccWBool
aplicarAccion AccWBool ge ts pila aux =
    let tRes  = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 24: AccOParam
aplicarAccion AccOParam ge ts pila aux =
    let tParam  = parametros (getAux aux 1)
        nParam  = numParametros (getAux aux 1)
        aux'    = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = TipoFuncion, parametros = tParam, numParametros = nParam})
    in return (ge, ts, pila, aux')

-- Regla 25: AccOVoid
aplicarAccion AccOVoid ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Regla 26: AccSId
aplicarAccion AccSId ge ts pila aux =
    let posId  = posTS (getAux aux 2)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        gInfo  = getAux aux 1
        gTipo  = tipo gInfo

        esLlamada = gTipo == TipoFuncion && idTipo == TipoFuncion
        paramId   = maybe [] parametros idInfo
        paramG    = parametros gInfo

        ge1
          | esLlamada && paramId /= paramG =
              errorSem
                  ("Los parametros de la llamada a la función son incorrectos, se esperaba los tipos "
                   ++ mostrarParametros paramId) ge
          | not esLlamada && idTipo /= gTipo =
              errorSem
                  ("Intentando asignar a una variable de tipo " ++ tipoAString idTipo
                   ++ " un valor de tipo " ++ tipoAString gTipo) ge
          | otherwise = ge

        aux' = liberarSimbolos aux 3
    in return (ge1, ts, pila, aux')

-- Regla 27: AccSWrite
aplicarAccion AccSWrite ge ts pila aux = 
    let eTipo = tipo (getAux aux 1)
        ge'   = if eTipo `notElem` [TipoEntero, TipoReal, TipoCadena]
                    then errorSem
                        ("La expresión write solo admite expresiones del tipo entero, real, cadena. Sin embargo está recibiendo un tipo "
                         ++ tipoAString eTipo) ge
                    else ge
        aux'  = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')

-- Regla 28: AccSRead
aplicarAccion AccSRead ge ts pila aux =
    let posId  = posTS (getAux aux 1)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo

        ge'    = if idTipo `notElem` [TipoEntero, TipoReal, TipoCadena]
                    then errorSem
                        ("La expresión read guarda valores del tipo entero, real, cadena. Sin embargo está intentando guardar en una variable de tipo "
                         ++ tipoAString idTipo) ge
                    else ge
        aux'   = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')
        
-- Regla 29: AccSReturn
aplicarAccion AccSReturn ge ts pila aux =
    let xTipo    = tipo (getAux aux 1)
        tipoRetS = tipoRet (getAux aux 3)
        ge'      = if xTipo /= tipoRetS
                    then errorSem
                        ("Tipo devuelto incorrecto, se esperaba devolver un tipo "
                         ++ tipoAString tipoRetS ++ " pero se devuelve un tipo " ++ tipoAString xTipo) ge
                    else ge
        aux'     = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')

-- Relga 30: AccGAsig
aplicarAccion AccGAsig ge ts pila aux =
    let tRes = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 2) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')


-- Relga 31: AccGParam
aplicarAccion AccGParam ge ts pila aux =
    let tParam = parametros (getAux aux 1)
        nParam = numParametros (getAux aux 1)
        aux'    = setAux (liberarSimbolos aux 3) 0 (\i -> i { tipo = TipoFuncion, parametros = tParam, numParametros = nParam})
    in return (ge, ts, pila, aux')

-- Relga 32: AccLParam
aplicarAccion AccLParam ge ts pila aux =
    let eTipo = tipo (getAux aux 1)
        qParam = parametros (getAux aux 0)
        nParam = numParametros (getAux aux 0) + 1
        aux'    = setAux (liberarSimbolos aux 2) 0 (\i -> i {parametros = eTipo:qParam, numParametros = nParam})
    in return (ge, ts, pila, aux')

-- Regla 33: AccLVoid
aplicarAccion AccLVoid ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')

-- Relga 34: AccQParam
aplicarAccion AccQParam ge ts pila aux =
    let eTipo = tipo (getAux aux 1)
        qParam = parametros (getAux aux 0)
        nParam = numParametros (getAux aux 0) + 1
        aux'    = setAux (liberarSimbolos aux 3) 0 (\i -> i {parametros = eTipo:qParam, numParametros = nParam})
    in return (ge, ts, pila, aux')

-- Regla 35: AccQVoid
aplicarAccion AccQVoid ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')

-- Relga 36: AccXTipo
aplicarAccion AccXTipo ge ts pila aux =
    let eTipo = tipo (getAux aux 0)
        aux'    = setAux (liberarSimbolos aux 1) 0 (\i -> i {tipo = eTipo})
    in return (ge, ts, pila, aux')

-- Regla 35: AccXVoid
aplicarAccion AccXVoid ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i {tipo = TipoVoid})
    in return (ge, ts, pila, aux')

-- Regla 38: AccBIf
aplicarAccion AccBIf ge ts pila aux =
    let eTipo = tipo (getAux aux 1)
        tipoRetB = tipoRet (getAux aux 4)
        msg   = "La expresión dentro del if debe ser de tipo lógico, sin embargo está evaluando una expresión de tipo "
              ++ tipoAString eTipo
        ge'   = if eTipo /= TipoLogico then errorSem msg ge else ge
        pila' = case pila of
                    []     -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoRet = tipoRetB } } : ss
    in return (ge', ts, pila', aux)

-- Regla 39: AccDec5
aplicarAccion AccDec5 ge ts pila aux =
    let aux' = liberarSimbolos aux 5
    in return (ge, ts, pila, aux')

-- Regla 40: AccZTipoRet
aplicarAccion AccZS ge ts pila aux =
    let tipoRetZ = tipoRet (getAux aux 0)
        pila' = case pila of
                    []     -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoRet = tipoRetZ } } : ss
    in return (ge, ts, pila', aux)

-- Regla 41: AccDec1
aplicarAccion AccDec1 ge ts pila aux =
    let aux' = liberarSimbolos aux 1
    in return (ge, ts, pila, aux')

-- Regla 42: AccZBloqueRet
aplicarAccion AccZBloqueRet ge ts pila aux =
    let tipoRetZ = tipoRet (getAux aux 0)
        pila' = case pila of
                    (p:s:ss) -> p : s { infoSem = (infoSem s) { tipoRet = tipoRetZ } } : ss
                    _    -> pila
    in return (ge, ts, pila', aux)

-- Regla 43: AccDec4
aplicarAccion AccDec4 ge ts pila aux =
    let aux' = liberarSimbolos aux 4
    in return (ge, ts, pila, aux')

-- Regla 44: AccITipoRet
aplicarAccion AccITipoRet ge ts pila aux =
    let tipoRetI = tipoRet (getAux aux 1)
        pila' = case pila of
                    (p:s:ss) -> p : s { infoSem = (infoSem s) { tipoRet = tipoRetI } } : ss
                    _        -> pila
    in return (ge, ts, pila', aux)

-- Regla 45: AccDec2
aplicarAccion AccDec2 ge ts pila aux =
    let aux' = liberarSimbolos aux 2
    in return (ge, ts, pila, aux')

-- Regla 47: AccDIf
aplicarAccion AccDIf ge ts pila aux =
    let eTipo = tipo (getAux aux 1)
        msg      = "La expresión dentro del if debe ser de tipo lógico, sin embargo está evaluando una expresión de tipo "
                 ++ tipoAString eTipo
        ge'      = if eTipo /= TipoLogico then errorSem msg ge else ge
        tipoRetD = tipoRet (getAux aux 1)
        pila' = case pila of
                    (p:s:t:c:ss) -> p : s { infoSem = (infoSem s) { tipoRet = tipoRetD } } : t : c { infoSem = (infoSem s) { tipoRet = tipoRetD } } : ss
                    _            -> pila
    in return (ge', ts, pila', aux)

-- Regla 48: AccDec8
aplicarAccion AccDec8 ge ts pila aux =
    let aux' = liberarSimbolos aux 8
    in return (ge, ts, pila, aux')
    
-- Regla 49: AccDTipoRet
aplicarAccion AccDTipoRet ge ts pila aux =
    let tipoRetD = tipoRet (getAux aux 1)
        pila' = case pila of
                    (p:s:ss) -> p : s { infoSem = (infoSem s) { tipoRet = tipoRetD } } : ss
                    _        -> pila
    in return (ge, ts, pila', aux)

-- Regla 50: AccDec3
aplicarAccion AccDec3 ge ts pila aux =
    let aux' = liberarSimbolos aux 3
    in return (ge, ts, pila, aux')

-- Regla 51: AccZonaDecl
aplicarAccion AccZonaDecl ge ts pila aux =
    let ts' = ts { zonaDecl = True }
    in return (ge, ts', pila, aux)

-- Regla 52: AccBLet 
aplicarAccion AccBLet ge ts pila aux =
    let posId = posTS (getAux aux 1)
        tTipo = tipo (getAux aux 2)
        ts1   = case posId of
                    Just pos -> asignarTipoYDespl pos tTipo ts
                    Nothing  -> ts
        ts2   = ts1 { zonaDecl = False }
        aux'  = liberarSimbolos aux 4
    in return (ge, ts2, pila, aux')

-- Regla 53: AccBTipoRet
aplicarAccion AccBTipoRet ge ts pila aux =
    let tRetB = tipoRet (getAux aux 0)
        pila' = case pila of
                    []      -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoRet = tRetB } } : ss
    in return (ge, ts, pila', aux)

-- Regla 54; AccTInt
aplicarAccion AccTInt ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = TipoEntero })
    in return (ge, ts, pila, aux')

-- Regla 55; AccTFloat
aplicarAccion AccTFloat ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = TipoReal })
    in return (ge, ts, pila, aux')

-- Regla 56; AccTInt
aplicarAccion AccTString ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = TipoCadena })
    in return (ge, ts, pila, aux')

-- Regla 57; AccTBool
aplicarAccion AccTBool ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = TipoLogico })
    in return (ge, ts, pila, aux')

-- Regla 58: AccFCrearTabla
aplicarAccion AccFCrearTabla ge ts pila aux =
    let posId  = posTS (getAux aux 0)
        hTipo  = tipo (getAux aux 1)
        ts1    = case posId of
                    Just pos -> updateInfo pos (\i -> i { tipo = TipoFuncion, tipoRet = hTipo }) ts
                    Nothing  -> ts
        ts2    = crearTablaLocal ts1
        pila'  = case pila of
                    (s:pA:a:pC:cA:c:ss) -> s:pA:a:pC:cA: c { infoSem = (infoSem s) { tipoRet = hTipo } } : ss
                    _       -> pila
    in return (ge, ts2, pila', aux)

-- Regla 59: AccFParamA 
aplicarAccion AccFParamA ge ts pila aux =
    let posIdFun = posTS (getAux aux 2)
        aParam   = parametros (getAux aux 0)
        aNum     = numParametros (getAux aux 0)
        ts1      = ts { zonaDecl = False }
        ts2      = case posIdFun of
                    Just pos -> updateInfo pos (\i -> i { parametros = aParam, numParametros = aNum }) ts1
                    Nothing  -> ts1
    in return (ge, ts2, pila, aux)

-- Regla 60: AccFLibTabla 
aplicarAccion AccFLibTabla ge ts pila aux =
    let ts'  = liberarTablaLocal ts
        aux' = liberarSimbolos aux 9
    in return (ge, ts', pila, aux')

-- Regla 61: AccHTipoT
aplicarAccion AccHTipoT ge ts pila aux =
    let tRes = tipo (getAux aux 0)
        aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')

-- Regla 62: AccHVoid
aplicarAccion AccHVoid ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Regla 70: AccAIdTipo
aplicarAccion AccAIdTipo ge ts pila aux =
    let posId = posTS (getAux aux 0)
        tTipo = tipo (getAux aux 1)
        ts'   = case posId of
                    Just pos -> asignarTipoYDespl pos tTipo ts
                    Nothing  -> ts
    in return (ge, ts', pila, liberarSimbolos aux 1)

-- Regla 63: AccAParamK
aplicarAccion AccAParamK ge ts pila aux =
    let kParam = parametros (getAux aux 0)
        kNum   = numParametros (getAux aux 0)
        tTipo  = tipo (getAux aux 2)
        aux'   = setAux (liberarSimbolos aux 3) 0
                    (\i -> i { parametros = tTipo : kParam, numParametros = kNum + 1 })
    in return (ge, ts, pila, aux')

-- Regla 64: AccAVoid
aplicarAccion AccAVoid ge ts pila aux =
    let aux' = setAux (liberarSimbolos aux 1) 0
                    (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')

-- Regla 65 
aplicarAccion AccKParam ge ts pila aux =
    let kParam = parametros (getAux aux 0)
        kNum   = numParametros (getAux aux 0)
        tTipo  = tipo (getAux aux 2)
        aux'   = setAux (liberarSimbolos aux 4) 0
                    (\i -> i { parametros = tTipo : kParam, numParametros = kNum + 1 })
    in return (ge, ts, pila, aux')

-- Regla 66: AccKVoid
aplicarAccion AccKVoid ge ts pila aux =
    let aux' = setAux aux 0 (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')

-- Caso 67: AccCTipoRet
aplicarAccion AccCTipoRet ge ts pila aux =
    let tRetC = tipoRet (getAux aux 0)
        pila' = case pila of
                    (b:c2:rest) ->
                        b  { infoSem = (infoSem b)  { tipoRet = tRetC } } :
                        c2 { infoSem = (infoSem c2) { tipoRet = tRetC } } :
                        rest
                    _   -> pila
    in return (ge, ts, pila', aux)

-- Regla 68: AccCrearTabla
-- La tabla global ya existe desde tablaInicial; no hace falta nada
aplicarAccion AccCrearTabla ge ts pila aux =
    return (ge, ts, pila, aux)

-- Regla 69: AccLibTabla
aplicarAccion AccLibTabla ge ts pila aux =
    let aux' = liberarSimbolos aux 1
    in return (ge, ts, pila, aux')

-- Resto de acciones: pendientes de implementar
aplicarAccion _  ge ts pila aux =
    return (ge, ts, pila, aux)
