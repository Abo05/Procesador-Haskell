module AccionesSem (aplicarAccion) where
-- TODO: Comprobar que esta bien y seguir

import Simbolos    (SimboloSem(..), InfoSem(..), Simbolo(..), Accion(..), TipoSem(..))
import GErrores    (GError, registrarErrorAsin, CodErr(..))
import TablaSimbolos (TablaSimbolos)
import Token       (Token(..))

-- Acceso a la pila auxiliar por índice desde la cima (1 = cima)
getAux :: [SimboloSem] -> Int -> InfoSem
getAux aux i = infoSem (aux !! (length aux - i))

-- Modificar un elemento de la pila auxiliar por índice desde la cima
-- TODO: Suponemos que siempre lo pedimos bien, hacer casos de error
setAux :: [SimboloSem] -> Int -> (InfoSem -> InfoSem) -> [SimboloSem]
setAux aux i f =
    let idx = length aux - i
        (antes, sim:despues) = splitAt idx aux
    in antes ++ [sim { infoSem = f (infoSem sim) }] ++ despues

-- Liberar n símbolos de la cima de la pila auxiliar
liberarSimbolos :: [SimboloSem] -> Int -> [SimboloSem]
liberarSimbolos aux n = drop n aux

aplicarAccion :: Accion
              -> GError -> TablaSimbolos
              -> [SimboloSem]   -- pila principal (para heredados)
              -> [SimboloSem]   -- pila auxiliar  (para sintetizados)
              -> Token
              -> IO (GError, TablaSimbolos, [SimboloSem], [SimboloSem])

-- Caso 1: AccER
-- E.tipo = if(E1.tipo = void || R.tipo = E1.tipo) then R.tipo else tipo_error
-- aux[-1] = E1, aux[-2] = R, aux[-3] = E (el no terminal que recibe el resultado)
aplicarAccion AccER ge ts pila aux _ =
    let e1Tipo = tipo (getAux aux 1)
        rTipo  = tipo (getAux aux 2)
        tRes   = if e1Tipo == TipoVoid || rTipo == e1Tipo
                    then rTipo
                    else TipoError
        ge'    = if tRes == TipoError
                    then registrarErrorAsin ErrNoTerminal "Error de tipos en expresión E" ge
                    else ge
        aux'   = setAux (liberarSimbolos aux 2) 1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Caso 2: AccE1And
-- E1.tipo = if(R.tipo = logico && E1_1.tipo ∈ {logico, void}) then logico else tipo_error
-- aux[-1] = E1_1, aux[-2] = R, aux[-3] = && (terminal), aux[-4] = E1
aplicarAccion AccE1And ge ts pila aux _ =
    let rTipo   = tipo (getAux aux 2)
        e1_1    = tipo (getAux aux 1)
        tRes    = if rTipo == TipoLogico && (e1_1 == TipoLogico || e1_1 == TipoVoid)
                    then TipoLogico
                    else TipoError
        ge'     = if tRes == TipoError
                    then registrarErrorAsin ErrNoTerminal "Error de tipos en expresión E1 con &&" ge
                    else ge
        aux'    = setAux (liberarSimbolos aux 3) 1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Caso 3: AccE1Void
-- E1.tipo = void
-- aux[-1] = E1
aplicarAccion AccE1Void ge ts pila aux _ =
    let aux' = setAux aux 1 (\i -> i { tipo = TipoVoid })
    in return (ge, ts, pila, aux')

-- Caso 4: AccRU
-- R.tipo = if R1.tipo = void then U.tipo
--          else if U.tipo = R1.tipo then logico
--          else tipo_error
-- aux[-1] = R1, aux[-2] = U, aux[-3] = R
aplicarAccion AccRU ge ts pila aux _ =
    let r1Tipo = tipo (getAux aux 1)
        uTipo  = tipo (getAux aux 2)
        tRes   = if r1Tipo == TipoVoid
                    then uTipo
                    else if uTipo == r1Tipo
                        then TipoLogico
                        else TipoError
        ge'    = if tRes == TipoError
                    then registrarErrorAsin ErrNoTerminal "Error de tipos en expresión R" ge
                    else ge
        aux'   = setAux (liberarSimbolos aux 2) 1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Caso 5: AccR1M  (operador >)
-- R1.tipo = if U.tipo ∈ {entero, real} && R1_2.tipo = void then U.tipo else tipo_error
-- aux[-1] = R1_2, aux[-2] = U, aux[-3] = > (terminal), aux[-4] = R1
aplicarAccion AccR1M ge ts pila aux _ =
    let r1_2Tipo = tipo (getAux aux 1)
        uTipo    = tipo (getAux aux 2)
        tRes     = if (uTipo == TipoEntero || uTipo == TipoReal) && r1_2Tipo == TipoVoid
                    then uTipo
                    else TipoError
        ge'      = if tRes == TipoError
                    then registrarErrorAsin ErrNoTerminal "Error de tipos en comparación >" ge
                    else ge
        aux'     = setAux (liberarSimbolos aux 3) 1 (\i -> i { tipo = tRes })
    in return (ge', ts, pila, aux')

-- Caso 38: AccBIf
-- Comprueba que E.tipo = logico, propaga tipoDevuelto heredado a Z
-- aux[-1] = E, aux[-5] = B (que tiene el tipoDevuelto heredado)
-- pila[-1] = Z (recibe el tipoDevuelto heredado)
aplicarAccion AccBIf ge ts pila aux _ =
    let eTipo   = tipo (getAux aux 2)   -- aux[-2] es E (tras ( E ))
        tdHer   = tipoDevuelto (getAux aux 5)  -- B tiene el tipoDevuelto heredado
        ge'     = if eTipo /= TipoLogico
                    then registrarErrorAsin ErrNoTerminal
                            "La condición del if debe ser de tipo lógico" ge
                    else ge
        -- Propagamos tipoDevuelto al Z que está en la cima de la pila principal
        pila'   = case pila of
                    []    -> pila
                    (s:ss) -> s { infoSem = (infoSem s) { tipoDevuelto = tdHer } } : ss
    in return (ge', ts, pila', aux)

-- Casos 68 y 69: AccCrearTabla y AccLibTabla
-- Por ahora sin implementación real de tablas anidadas
aplicarAccion AccCrearTabla ge ts pila aux _ =
    return (ge, ts, pila, aux)

aplicarAccion AccLibTabla ge ts pila aux _ =
    let aux' = liberarSimbolos aux 1
    in return (ge, ts, pila, aux')

-- Caso 67: AccCTipoRet (atributo heredado)
-- B.tipoRet = C.tipoRet, C2.tipoRet = C.tipoRet
-- pila[-1] = B, pila[-2] = C2
aplicarAccion AccCTipoRet ge ts pila aux _ =
    let tdHer = tipoDevuelto (getAux aux 1)
        pila' = case pila of
                    (b:c2:rest) ->
                        b  { infoSem = (infoSem b)  { tipoDevuelto = tdHer } } :
                        c2 { infoSem = (infoSem c2) { tipoDevuelto = tdHer } } :
                        rest
                    _ -> pila
    in return (ge, ts, pila', aux)

-- Caso 69: AccLibTabla (liberar tabla de función)
aplicarAccion AccFLibTabla ge ts pila aux _ =
    let aux' = liberarSimbolos aux 9
    in return (ge, ts, pila, aux')

-- Resto de acciones: pendientes de implementar
aplicarAccion _ ge ts pila aux _ =
    return (ge, ts, pila, aux)
