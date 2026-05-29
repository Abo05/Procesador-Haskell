module GErrores where

-- TODO: Esto investigarlo, pero investigar más
data GError = GError
    { linea :: Int
    , errores :: [Error]
    } deriving (Show)

data Error = Error
    { errLinea :: Int
    , errCodigo :: CodErr
    , errMensaje :: String
    } deriving (Eq)

--TODO: ver si es menjor cambiar a Text
--al hacer el "struct", se crean los getters automaticamente
instance Show Error where
    show e = "error en la línea " ++ show(errLinea e) ++ ": " ++ errMensaje e

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

-- Cada vez crea una nueva variable de GErrores
registrarError :: CodErr -> GError -> GError
registrarError cod ge =
        let err = Error
                { errLinea    = linea ge
                , errCodigo   = cod
                , errMensaje  = mensajeError cod
                }
        in ge { errores = err : errores ge }

hayErrores :: GError -> Bool
hayErrores ge = not (null(errores ge))

-- Los errores se acumularon en orden inverso
listarErrores :: GError -> [Error]
listarErrores = reverse . errores

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
             deriving Eq

-- El número tiene que ver con el estado en el que me encuentro
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
-- mensajeError _  = "error desconocido"
-- TODO: Ver si tiene sentido esa línea
