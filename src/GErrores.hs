module gerrores where

-- TODO: Esto sería una mónada, pero investigar más
data GError = Gerror
    { linea :: int
    , errores :: [Error]
    } deriving (Show)

data Error = Error
    { errlinea :: int
    , errcodigo :: int
    , errmensaje :: string
    } deriving (eq)

--TODO: cambiar a Text
--al hacer el "struct", se crean los getters automaticamente
instance show error where
    show e = "error en la línea " ++ show(errlinea e)
    ++ ": " ++ errmensaje e

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
