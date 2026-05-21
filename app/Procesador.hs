module Main (main) where

import qualified MyLib (someFunc)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Alex (getToken)
import Token (Token(..))


-- Pasamos de un fichero a un string con el contenido del fichero
-- Esta función es lazy, luego convierte el fichero según sea necesario
main :: IO ()
main = do
  args <- getArgs
  fichero <- case args of
    [f] -> readFile f
    _ -> do
        hPutStrLn stderr "Uso: lexer <fichero>"
        exitWith (ExitFailure 1)
  MyLib.someFunc

--TODO: Una función que vaya recibiendo tokens y vaya escribiendo estos
