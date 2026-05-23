module GErrores where

-- TODO: Esto investigarlo, pero investigar más
data GError = GError
    { linea :: Int
    , errores :: [Error]
    } deriving (Show)

data Error = Error
    { errLinea :: Int
    , errCodigo :: Int
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
registrarError :: Int -> GError -> GError
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

-- El número tiene que ver con el estado en el que me encuentro
mensajeError :: Int -> String
mensajeError 0  = "carácter \\ o '.' no esperado en estado inicial"
mensajeError 2  = "número real con parte decimal faltante"
mensajeError 4  = "carácter TAB no permitido en cadena"
mensajeError 5  = "cadena no cerrada correctamente"
mensajeError 6  = "secuencia de escape no válida"
mensajeError 9  = "operador and incompleto"
mensajeError 10 = "otra / esperada para inicio de comentario"
mensajeError 11 = "carácter desconocido"
mensajeError 21 = "valor entero excede el máximo permitido"
mensajeError 24 = "valor real excede el máximo permitido"
mensajeError 28 = "cadena excede el número máximo de caracteres"
mensajeError _  = "error desconocido"
