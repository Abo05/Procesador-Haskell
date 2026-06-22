module AccionesSem (aplicarAccion) where

import Simbolos         (SimboloSem(..), InfoSem(..), Accion(..), TipoSem(..), tipoAString, mostrarParametros)
import GErrores         (GError, registrarErrorAsin, CodErr(..))
import TablaSimbolos    (TablaSimbolos (numTablas), liberarTablaLocal, getInfo, zonaDecl, asignarTipoYDespl, updateInfo, crearTablaLocal, tablaLocal, formatearTabla)
import System.IO        (Handle, hPutStr)

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
              -> Handle
              -> IO (GError, TablaSimbolos, [SimboloSem], [SimboloSem])

-- Regla 1: AccER
aplicarAccion AccER ge ts pila aux _ =
    let e1Tipo = tipo (getAux aux e1)
        rTipo  = tipo (getAux aux r)
        tRes   = if e1Tipo == TipoVoid || rTipo == e1Tipo
                    then rTipo
                    else TipoError
        msg    = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
               ++ tipoAString rTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString e1Tipo
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) e (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where   r = 1
                e1 = 0
                e = 0

-- Regla 2: AccE1And
aplicarAccion AccE1And ge ts pila aux _ =
    let rTipo  = tipo (getAux aux r)
        e1_1   = tipo (getAux aux e1_1_)
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
        aux'   = setAux (liberarSimbolos aux 3) e1 (\i -> i { tipo = tRes })
    in return (ge2, ts, pila, aux')
        where r = 1
              e1_1_ = 0
              e1 = 0

-- Regla 3: AccE1Void
aplicarAccion AccE1Void ge ts pila aux _ =
    let aux' = setAux aux e1 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')
        where e1 = 0

-- Regla 4: AccRU
aplicarAccion AccRU ge ts pila aux _ =
    let r1Tipo = tipo (getAux aux r1)
        uTipo  = tipo (getAux aux u)
        tRes   = if r1Tipo == TipoVoid
                    then uTipo
                    else if uTipo == r1Tipo then TipoLogico else TipoError
        msg    = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
               ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1Tipo
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) r (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u = 1
              r1 = 0
              r = 0

-- Regla 5: AccR1M  (operador >)
aplicarAccion AccR1M ge ts pila aux _ =
    let r1_2Tipo = tipo (getAux aux r1_2)
        uTipo    = tipo (getAux aux u)
        tRes     = if (uTipo == TipoEntero || uTipo == TipoReal) && (r1_2Tipo == TipoVoid || r1_2Tipo == uTipo)
                    then uTipo else TipoError
        msg      = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1_2Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) r1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u = 1
              r1_2 = 0
              r1 = 0

-- Regla 6: AccR1MI  (operador >=)
aplicarAccion AccR1MI ge ts pila aux _ =
    let r1_2Tipo = tipo (getAux aux r1_2)
        uTipo    = tipo (getAux aux u)
        tRes     = if (uTipo == TipoEntero || uTipo == TipoReal) && (r1_2Tipo == TipoVoid || r1_2Tipo == uTipo)
                    then uTipo else TipoError
        msg      = "Se están comparando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString uTipo ++ " ,mientras que la segunda es del tipo " ++ tipoAString r1_2Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 3) r1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u = 1
              r1_2 = 0
              r1 = 0

-- Regla 7: AccR1Void
aplicarAccion AccR1Void ge ts pila aux _ =
    let aux' = setAux aux r1 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')
        where r1 = 0

-- Regla 8: AccUV
aplicarAccion AccUV ge ts pila aux _ =
    let u1Tipo = tipo (getAux aux u1)
        vTipo = tipo (getAux aux v)
        tRes     = if (u1Tipo == TipoVoid || vTipo == u1Tipo)
                    then vTipo else TipoError
        msg      = "Se están operando dos expresiones de distinto tipo, la primera expresión es del tipo "
                 ++ tipoAString vTipo ++ ", mientras que la segunda es del tipo " ++ tipoAString u1Tipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) u (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u1 = 0
              v = 1
              u = 0

-- Regla 9: AccU1Suma
aplicarAccion AccU1Suma ge ts pila aux _ =
    let u1Tipo = tipo (getAux aux u1_1)
        vTipo = tipo (getAux aux v)
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
        aux'     = setAux (liberarSimbolos aux 3) u1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u1_1 = 0
              v = 1
              u1 = 0

-- Regla 10: AccU1Suma
aplicarAccion AccU1Resta ge ts pila aux _ =
    let u1Tipo = tipo (getAux aux u1_1)
        vTipo = tipo (getAux aux v)
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
        aux'     = setAux (liberarSimbolos aux 3) u1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where u1_1 = 0
              v = 1
              u1 = 0

-- Regla 11: AccU1Void
aplicarAccion AccU1Void ge ts pila aux _ =
    let aux' = setAux aux u1 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')
        where u1 = 0

-- Regla 12: AccMasW
aplicarAccion AccMasW ge ts pila aux _ =
    let wTipo = tipo (getAux aux w)
        tRes     = if wTipo == TipoEntero || wTipo == TipoReal
                    then wTipo else TipoError
        msg      = "La suma unaria solo se puede utilizar con tipos enteros o reales, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) v (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where v = 0
              w = 0

-- Regla 13: AccMenosW
aplicarAccion AccMenosW ge ts pila aux _ =
    let wTipo = tipo (getAux aux w)
        tRes     = if wTipo == TipoEntero || wTipo == TipoReal
                    then wTipo else TipoError
        msg      = "La resta unaria solo se puede utilizar con tipos enteros o reales, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) v (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where v = 0
              w = 0

-- Regla 14: AccNotW
aplicarAccion AccNotW ge ts pila aux _ =
    let wTipo = tipo (getAux aux w)
        tRes     = if wTipo == TipoLogico
                    then TipoLogico else TipoError
        msg      = "La operación de negación solo se puede utilizar con tipos lógicos, sin embargo, se está utilizando un tipo "
                 ++ tipoAString wTipo
        ge'      = if tRes == TipoError then errorSem msg ge else ge
        aux'     = setAux (liberarSimbolos aux 2) v (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where w = 0
              v = 0

-- Regla 15: AccVDec
aplicarAccion AccVDec ge ts pila aux _ =
    let posId  = posTS (getAux aux identificador)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        tRes   = if idTipo == TipoEntero || idTipo == TipoReal
                    then idTipo
                    else TipoError
        msg    = "Los parametros de la operación de decremento son incorrectos, deben ser un identificador entero o real"
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) v (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where identificador = 0
              v = 0
    
-- Regla 16: AccVW
aplicarAccion AccVW ge ts pila aux _ =
    let tRes  = tipo (getAux aux w)
        aux'    = setAux (liberarSimbolos aux 1) v (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')
        where v = 0
              w = 0

-- Regla 17: AccWId
aplicarAccion AccWId ge ts pila aux _ =
    let posId  = posTS (getAux aux identificador)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        oTipo  = tipo (getAux aux o)
        tRes   = if oTipo == TipoVoid
                    then idTipo
                    else if parametros (getAux aux o) == maybe [] parametros idInfo
                            then maybe TipoError tipoRet idInfo
                            else TipoError
        msg    = ("Los parametros de la llamada a la función son incorrectos, se esperaba los tipos "
                   ++ mostrarParametros (maybe [] parametros idInfo))
        ge'    = if tRes == TipoError then errorSem msg ge else ge
        aux'   = setAux (liberarSimbolos aux 2) w (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')
        where identificador = 1
              o = 0
              w = 0

-- Regla 18: AccWTipo
aplicarAccion AccWTipo ge ts pila aux _ =
    let tRes  = tipo (getAux aux e)
        aux'    = setAux (liberarSimbolos aux 3) w (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')
        where e = 1
              w = 0

-- Regla 19: AccWInt
aplicarAccion AccWInt ge ts pila aux _ =
    let aux'    = setAux (liberarSimbolos aux 1) w (\i -> i { tipo = TipoEntero })
    in return (ge, ts, pila, aux')
        where w = 0

-- Regla 20: AccWFloat
aplicarAccion AccWFloat ge ts pila aux _ =
    let aux'    = setAux (liberarSimbolos aux 1) w (\i -> i { tipo = TipoReal })
    in return (ge, ts, pila, aux')
        where w = 0

-- Regla 21: AccWString
aplicarAccion AccWString ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) w (\i -> i { tipo = TipoCadena })
    in return (ge, ts, pila, aux')
        where w = 0

-- Regla 22: AccWBool
aplicarAccion AccWBool ge ts pila aux _ =
    let aux'    = setAux (liberarSimbolos aux 1) w (\i -> i { tipo = TipoLogico })
    in return (ge, ts, pila, aux')
        where w = 0

-- Regla 24: AccOParam
aplicarAccion AccOParam ge ts pila aux _ =
    let tParam  = parametros (getAux aux l)
        nParam  = numParametros (getAux aux l)
        aux'    = setAux (liberarSimbolos aux 3) o (\i -> i { tipo = TipoFuncion, parametros = tParam, numParametros = nParam})
    in return (ge, ts, pila, aux')
        where l = 1
              o = 0

-- Regla 25: AccOVoid
aplicarAccion AccOVoid ge ts pila aux _ =
    let aux' = setAux aux o (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')
        where o = 0

-- Regla 26: AccSId
aplicarAccion AccSId ge ts pila aux _ =
    let posId  = posTS (getAux aux identificador)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo
        gInfo  = getAux aux g
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
        where identificador = 2
              g = 1

-- Regla 27: AccSWrite
aplicarAccion AccSWrite ge ts pila aux _ = 
    let eTipo = tipo (getAux aux e)
        ge'   = if eTipo `notElem` [TipoEntero, TipoReal, TipoCadena]
                    then errorSem
                        ("La expresión write solo admite expresiones del tipo entero, real, cadena. Sin embargo está recibiendo un tipo "
                         ++ tipoAString eTipo) ge
                    else ge
        aux'  = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')
        where e = 1

-- Regla 28: AccSRead
aplicarAccion AccSRead ge ts pila aux _ =
    let posId  = posTS (getAux aux identificador)
        idInfo = posId >>= \p -> getInfo p ts
        idTipo = maybe TipoError tipo idInfo

        ge'    = if idTipo `notElem` [TipoEntero, TipoReal, TipoCadena]
                    then errorSem
                        ("La expresión read guarda valores del tipo entero, real, cadena. Sin embargo está intentando guardar en una variable de tipo "
                         ++ tipoAString idTipo) ge
                    else ge
        aux'   = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')
        where identificador = 1
        
-- Regla 29: AccSReturn
aplicarAccion AccSReturn ge ts pila aux _ =
    let xTipo    = tipo (getAux aux x)
        tipoRetS = tipoRet (getAux aux s)
        ge'      = if xTipo /= tipoRetS
                    then errorSem
                        ("Tipo devuelto incorrecto, se esperaba devolver un tipo "
                         ++ tipoAString tipoRetS ++ " pero se devuelve un tipo " ++ tipoAString xTipo) ge
                    else ge
        aux'     = liberarSimbolos aux 3
    in return (ge', ts, pila, aux')
        where x = 1
              s = 3

-- Relga 30: AccGAsig
aplicarAccion AccGAsig ge ts pila aux _ =
    let tRes = tipo (getAux aux e)
        aux'    = setAux (liberarSimbolos aux 2) g (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')
        where e = 0
              g = 0


-- Relga 31: AccGParam
aplicarAccion AccGParam ge ts pila aux _ =
    let tParam = parametros (getAux aux l)
        nParam = numParametros (getAux aux l)
        aux'    = setAux (liberarSimbolos aux 3) g (\i -> i { tipo = TipoFuncion, parametros = tParam, numParametros = nParam})
    in return (ge, ts, pila, aux')
        where l = 1
              g = 0

-- Relga 32: AccLParam
aplicarAccion AccLParam ge ts pila aux _ =
    let eTipo = tipo (getAux aux e)
        qParam = parametros (getAux aux q)
        nParam = numParametros (getAux aux q) + 1
        aux'    = setAux (liberarSimbolos aux 2) l (\i -> i {parametros = eTipo:qParam, numParametros = nParam})
    in return (ge, ts, pila, aux')
        where e = 1
              q = 0
              l = 0

-- Regla 33: AccLVoid
aplicarAccion AccLVoid ge ts pila aux _ =
    let aux' = setAux aux l (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')
        where l = 0

-- Relga 34: AccQParam
aplicarAccion AccQParam ge ts pila aux _ =
    let eTipo = tipo (getAux aux e)
        qParam = parametros (getAux aux q_2)
        nParam = numParametros (getAux aux q_2) + 1
        aux'    = setAux (liberarSimbolos aux 3) q (\i -> i {parametros = eTipo:qParam, numParametros = nParam})
    in return (ge, ts, pila, aux')
        where e = 1
              q_2 = 0
              q = 0

-- Regla 35: AccQVoid
aplicarAccion AccQVoid ge ts pila aux _ =
    let aux' = setAux aux q (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')
        where q = 0

-- Relga 36: AccXTipo
aplicarAccion AccXTipo ge ts pila aux _ =
    let eTipo = tipo (getAux aux e)
        aux'    = setAux (liberarSimbolos aux 1) x (\i -> i {tipo = eTipo})
    in return (ge, ts, pila, aux')
        where e = 0
              x = 0

-- Regla 35: AccXVoid
aplicarAccion AccXVoid ge ts pila aux _ =
    let aux' = setAux aux x (\i -> i {tipo = TipoVoid})
    in return (ge, ts, pila, aux')
        where x = 0

-- Regla 38: AccBIf
aplicarAccion AccBIf ge ts pila aux _ =
    let eTipo = tipo (getAux aux e)
        tipoRetB = tipoRet (getAux aux b)
        msg   = "La expresión dentro del if debe ser de tipo lógico, sin embargo está evaluando una expresión de tipo "
              ++ tipoAString eTipo
        ge'   = if eTipo /= TipoLogico then errorSem msg ge else ge
        pila' = case pila of
                    []     -> pila
                    (z:ss) -> z { infoSem = (infoSem z) { tipoRet = tipoRetB } } : ss
    in return (ge', ts, pila', aux)
        where e = 1
              b = 4

-- Regla 39: AccDec5
aplicarAccion AccDec5 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 5
    in return (ge, ts, pila, aux')

-- Regla 40: AccZTipoRet
aplicarAccion AccZS ge ts pila aux _ =
    let tipoRetZ = tipoRet (getAux aux z)
        pila' = case pila of
                    []     -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoRet = tipoRetZ } } : ss
    in return (ge, ts, pila', aux)
        where z = 0

-- Regla 41: AccDec1
aplicarAccion AccDec1 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 1
    in return (ge, ts, pila, aux')

-- Regla 42: AccZBloqueRet
aplicarAccion AccZBloqueRet ge ts pila aux _ =
    let tipoRetZ = tipoRet (getAux aux z)
        pila' = case pila of
                    (cA:c:cC:i:ss) -> cA : c { infoSem = (infoSem i) { tipoRet = tipoRetZ } } : cC: i { infoSem = (infoSem i) { tipoRet = tipoRetZ } }: ss
                    _    -> pila
    in return (ge, ts, pila', aux)
        where z = 0

-- Regla 43: AccDec4
aplicarAccion AccDec4 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 4
    in return (ge, ts, pila, aux')

-- Regla 44: AccITipoRet
aplicarAccion AccITipoRet ge ts pila aux _ =
    let tipoRetI = tipoRet (getAux aux i)
        pila' = case pila of
                    (d:ss) ->  d { infoSem = (infoSem d) { tipoRet = tipoRetI } } : ss
                    _        -> pila
    in return (ge, ts, pila', aux)
        where i = 1

-- Regla 45: AccDec2
aplicarAccion AccDec2 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 2
    in return (ge, ts, pila, aux')

-- Regla 47: AccDIf
aplicarAccion AccDIf ge ts pila aux _ =
    let eTipo = tipo (getAux aux e)
        msg      = "La expresión dentro del if debe ser de tipo lógico, sin embargo está evaluando una expresión de tipo "
                 ++ tipoAString eTipo
        ge'      = if eTipo /= TipoLogico then errorSem msg ge else ge
        tipoRetD = tipoRet (getAux aux d)
        pila' = case pila of
                    (cA:c:cC:i:ss) -> cA : c { infoSem = (infoSem c) { tipoRet = tipoRetD } } : cC : i { infoSem = (infoSem i) { tipoRet = tipoRetD } } : ss
                    _            -> pila
    in return (ge', ts, pila', aux)
        where e = 1
              d = 4

-- Regla 48: AccDec8
aplicarAccion AccDec8 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 8
    in return (ge, ts, pila, aux')
    
-- Regla 49: AccDTipoRet
aplicarAccion AccDTipoRet ge ts pila aux _ =
    let tipoRetD = tipoRet (getAux aux d)
        pila' = case pila of
                    (cA:c:ss) -> cA : c { infoSem = (infoSem c) { tipoRet = tipoRetD } } : ss
                    _        -> pila
    in return (ge, ts, pila', aux)
        where d = 0

-- Regla 50: AccDec3
aplicarAccion AccDec3 ge ts pila aux _ =
    let aux' = liberarSimbolos aux 3
    in return (ge, ts, pila, aux')

-- Regla 51: AccZonaDecl
aplicarAccion AccZonaDecl ge ts pila aux _ =
    let ts' = ts { zonaDecl = True }
    in return (ge, ts', pila, aux)

-- Regla 52: AccBLet 
aplicarAccion AccBLet ge ts pila aux _ =
    let posId = posTS (getAux aux identificador)
        tTipo = tipo (getAux aux t)
        ts1   = case posId of
                    Just pos -> asignarTipoYDespl pos tTipo ts
                    Nothing  -> ts
        ts2   = ts1 { zonaDecl = False }
        aux'  = liberarSimbolos aux 4
    in return (ge, ts2, pila, aux')
        where identificador = 1
              t = 2

-- Regla 53: AccBTipoRet
aplicarAccion AccBTipoRet ge ts pila aux _ =
    let tRetB = tipoRet (getAux aux b)
        pila' = case pila of
                    []      -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoRet = tRetB } } : ss
    in return (ge, ts, pila', aux)
        where b = 0

-- Regla 54; AccTInt
aplicarAccion AccTInt ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) t (\i -> i { tipo = TipoEntero })
    in return (ge, ts, pila, aux')
        where t = 0

-- Regla 55; AccTFloat
aplicarAccion AccTFloat ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) t (\i -> i { tipo = TipoReal })
    in return (ge, ts, pila, aux')
        where t = 0

-- Regla 56; AccTString
aplicarAccion AccTString ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) t (\i -> i { tipo = TipoCadena })
    in return (ge, ts, pila, aux')
        where t = 0

-- Regla 57; AccTBool
aplicarAccion AccTBool ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) t (\i -> i { tipo = TipoLogico })
    in return (ge, ts, pila, aux')
        where t = 0

-- Regla 58: AccFCrearTabla
aplicarAccion AccFCrearTabla ge ts pila aux _ =
    let posId  = posTS (getAux aux identificador)
        hTipo  = tipo (getAux aux h)
        ts1    = case posId of
                    Just pos -> updateInfo pos (\i -> i { tipo = TipoFuncion, tipoRet = hTipo }) ts
                    Nothing  -> ts
        ts2    = crearTablaLocal ts1
        pila'  = case pila of
                    (pA:a:accion:pC:cA:c:ss) -> pA:a:accion:pC:cA: c { infoSem = (infoSem c) { tipoRet = hTipo } } : ss
                    _       -> pila
    in return (ge, ts2, pila', aux)
        where identificador = 0
              h = 1

-- Regla 59: AccFParamA 
aplicarAccion AccFParamA ge ts pila aux _ =
    let posIdFun = posTS (getAux aux identificador)
        aParam   = parametros (getAux aux a)
        aNum     = numParametros (getAux aux a)
        ts1      = ts { zonaDecl = False }
        ts2      = case posIdFun of
                    Just pos -> updateInfo pos (\i -> i { parametros = aParam, numParametros = aNum }) ts1
                    Nothing  -> ts1
    in return (ge, ts2, pila, aux)
        where identificador = 2
              a = 0

-- Regla 60: AccFLibTabla 
aplicarAccion AccFLibTabla ge ts pila aux hLocales = do
    case tablaLocal ts of
        Just tl -> do
            hPutStr hLocales ("CONTENIDO DE LA TABLA # " ++ show (numTablas ts) ++ " :\n")
            hPutStr hLocales (formatearTabla tl)
        Nothing -> return ()
    let ts'  = liberarTablaLocal ts
        aux' = liberarSimbolos aux 9
    return (ge, ts', pila, aux')

-- Regla 61: AccHTipoT
aplicarAccion AccHTipoT ge ts pila aux _ =
    let tRes = tipo (getAux aux t)
        aux' = setAux (liberarSimbolos aux 1) h (\i -> i { tipo = tRes })
    in return (ge, ts, pila, aux')
        where t = 0
              h = 0

    
-- Regla 62: AccHVoid
aplicarAccion AccHVoid ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) h (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')
        where h = 0

-- Regla 70: AccAIdTipo
aplicarAccion AccAIdTipo ge ts pila aux _ =
    let posId = posTS (getAux aux identificador)
        tTipo = tipo (getAux aux t)
        ts'   = case posId of
                    Just pos -> asignarTipoYDespl pos tTipo ts
                    Nothing  -> ts
    in return (ge, ts', pila, aux)
        where identificador = 0
              t = 1

-- Regla 63: AccAParamK
aplicarAccion AccAParamK ge ts pila aux _ =
    let kParam = parametros (getAux aux k)
        kNum   = numParametros (getAux aux k)
        tTipo  = tipo (getAux aux t)
        aux'   = setAux (liberarSimbolos aux 3) a
                    (\i -> i { parametros = tTipo : kParam, numParametros = kNum + 1 })
    in return (ge, ts, pila, aux')
        where k = 0
              t = 2
              a = 0

-- Regla 64: AccAVoid
aplicarAccion AccAVoid ge ts pila aux _ =
    let aux' = setAux (liberarSimbolos aux 1) a
                    (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')
        where a = 0

-- Regla 65 
aplicarAccion AccKParam ge ts pila aux _ =
    let kParam = parametros (getAux aux k_2)
        kNum   = numParametros (getAux aux k)
        tTipo  = tipo (getAux aux t)
        aux'   = setAux (liberarSimbolos aux 4) k
                    (\i -> i { parametros = tTipo : kParam, numParametros = kNum + 1 })
    in return (ge, ts, pila, aux')
        where k_2 = 0
              t = 2
              k = 0

-- Regla 66: AccKVoid
aplicarAccion AccKVoid ge ts pila aux _ =
    let aux' = setAux aux k (\i -> i { parametros = [], numParametros = 0 })
    in return (ge, ts, pila, aux')
        where k = 0

-- Caso 67: AccCTipoRet
aplicarAccion AccCTipoRet ge ts pila aux _ =
    let tRetC = tipoRet (getAux aux c)
        pila' = case pila of
                    (b:c2:rest) ->
                        b  { infoSem = (infoSem b)  { tipoRet = tRetC } } :
                        c2 { infoSem = (infoSem c2) { tipoRet = tRetC } } :
                        rest
                    _   -> pila
    in return (ge, ts, pila', aux)
        where c = 0

-- Regla 68: AccCrearTabla
-- La tabla global ya existe desde tablaInicial; no hace falta nada
aplicarAccion AccCrearTabla ge ts pila aux _ =
    return (ge, ts, pila, aux)

-- Regla 69: AccLibTabla
aplicarAccion AccLibTabla ge ts pila aux _ =
    let aux' = liberarSimbolos aux 1
    in return (ge, ts, pila, aux')

