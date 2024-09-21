module Auxiliares 
  ( User(..)
  , getUsers
  ) where

import Control.Applicative ((<*>), (<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Prelude hiding (filter)

-- Estructura de datos para los usuarios
data User = User
  { idUsuario :: String
  , nombreCompleto :: String
  , puesto :: String
  }
  deriving (Show, Eq)

type ErrorMsg = String

{- Inicio: Leer csv usuarios -------------------------------------------------------------------------------------------------}

{- 
/*****Nombre****************************************
 * instance FromNamedRecord User
 *****Descripción***********************************
 * Define cómo se obtiene un User a partir de un registro (fila CSV). 
 * Implementa la instancia de la clase de tipo `FromNamedRecord` 
 * para el tipo `User`, especificando cómo extraer los valores 
 * de las columnas del CSV basándose en los nombres de las columnas.
 *****Parámetros************************************
 * @record: Registro CSV que contiene los datos del usuario.
 *****Retorno***************************************
 * @User: Un valor del tipo `User` creado a partir de los datos del registro CSV.
 ***************************************************/
-}
instance FromNamedRecord User where
  parseNamedRecord record = User
    <$> record .: B8.pack "id"
    <*> record .: B8.pack "nombre completo"
    <*> record .: B8.pack "puesto"

type CsvDataUser = (Header, V.Vector User)

{- 
/*****Nombre****************************************
 * parseCsv
 *****Descripción***********************************
 * Lee el archivo CSV desde la ruta dada y decodifica su contenido.
 * Verifica si el archivo existe y, si es así, intenta leer y decodificar 
 * el archivo CSV. Si el archivo no existe, retorna un mensaje de error.
 *****Parámetros************************************
 * @filePath: Ruta del archivo CSV.
 *****Retorno***************************************
 * @IO (Either ErrorMsg CsvData): Un valor `Either` que puede contener un mensaje de error 
 * o los datos decodificados del archivo CSV.
 ***************************************************/
-}
parseCsvUser :: FilePath -> IO (Either ErrorMsg CsvDataUser)
parseCsvUser filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      csvDataUser <- BL.readFile filePath
      return $ decodeByName csvDataUser
    else return . Left $ printf "The file %s does not exist" filePath

{- 
/*****Nombre****************************************
 * getUsers
 *****Descripción***********************************
 * Obtiene los usuarios del archivo CSV llamando a `parseCsv` 
 * con la ruta del archivo. Maneja el resultado de la decodificación
 * y devuelve los usuarios en un valor `Either`.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (Either ErrorMsg (V.Vector User)): Un valor `Either` que puede contener un mensaje 
 * de error o un vector de usuarios leídos del archivo CSV.
 ***************************************************/
-}
getUsers :: IO (Either ErrorMsg (V.Vector User))
getUsers = do
  let filePath = "Archivos del sistema/usuarios.csv"
  result <- parseCsvUser filePath
  case result of
    Left err -> return $ Left err
    Right (_, users) -> return $ Right users
    
{- Fin: Leer csv usuarios -------------------------------------------------------------------------------------------------}
