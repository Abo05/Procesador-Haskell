module AccionesSem where

import Simbolos
import GErrores

accion :: [Simbolo] -> [Simbolo] -> GError -> ([Simbolo], [Simbolo], GError)
accion [] [] ge = ([],[],ge)
