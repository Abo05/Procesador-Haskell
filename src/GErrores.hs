module GErrores where

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
mensajeError _            = "este error no debería salir. El sintáctico llamó a esta función"

