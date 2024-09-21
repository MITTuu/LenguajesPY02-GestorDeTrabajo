module Operativas 
  ( Mobiliario(..)
  , getMobiliarios
  ) where

import Prelude hiding (filter)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Control.Applicative ((<*>), (<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Text.Printf (printf)
import System.IO (writeFile)

-- Estructura de datos para los mobiliarios de sala
data Mobiliario = Mobiliario
  { codigo :: String
  , nombre :: String
  , descripcion :: String
  , tipo :: String
  }
  deriving (Show, Eq)

type ErrorMsg = String


{- Inicio: Leer csv mobiliario -------------------------------------------------------------------------------------------------}

{- 
/*****Nombre****************************************
 * instance FromNamedRecord Mobiliario
 *****Descripción***********************************
 * Define cómo se obtiene un Mobiliario a partir de un registro (fila CSV). 
 * Implementa la instancia de la clase de tipo `FromNamedRecord` 
 * para el tipo `Mobiliario`, especificando cómo extraer los valores 
 * de las columnas del CSV basándose en los nombres de las columnas.
 *****Parámetros************************************
 * @record: Registro CSV que contiene los datos del mobiliario.
 *****Retorno***************************************
 * @Mobiliario: Un valor del tipo `Mobiliario` creado a partir de los datos del registro CSV.
 ***************************************************/
-}
instance FromNamedRecord Mobiliario where
  parseNamedRecord record = Mobiliario
    <$> record .: B8.pack "codigo"
    <*> record .: B8.pack "nombre"
    <*> record .: B8.pack "descripcion"
    <*> record .: B8.pack "tipo"

type CsvDataMobiliario = (Header, V.Vector Mobiliario)

{- 
/*****Nombre****************************************
 * parseCsvMobiliario
 *****Descripción***********************************
 * Lee el archivo CSV desde la ruta dada y decodifica su contenido.
 * Verifica si el archivo existe y, si es así, intenta leer y decodificar 
 * el archivo CSV. Si el archivo no existe, retorna un mensaje de error.
 *****Parámetros************************************
 * @filePath: Ruta del archivo CSV.
 *****Retorno***************************************
 * @IO (Either ErrorMsg CsvDataMobiliario): Un valor `Either` que puede contener un mensaje de error 
 * o los datos decodificados del archivo CSV.
 ***************************************************/
-}
parseCsvMobiliario :: FilePath -> IO (Either ErrorMsg CsvDataMobiliario)
parseCsvMobiliario filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      csvDataMobiliario <- BL.readFile filePath
      return $ decodeByName csvDataMobiliario
    else return . Left $ printf "The file %s does not exist" filePath

{- 
/*****Nombre****************************************
 * getMobiliarios
 *****Descripción***********************************
 * Obtiene el mobiliario del archivo CSV llamando a `parseCsv` 
 * con la ruta del archivo. Maneja el resultado de la decodificación
 * y devuelve los usuarios en un valor `Either`.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (Either ErrorMsg (V.Vector User)): Un valor `Either` que puede contener un mensaje 
 * de error o un vector de usuarios leídos del archivo CSV.
 ***************************************************/
-}
getMobiliarios :: FilePath -> IO (Either ErrorMsg (V.Vector Mobiliario))
getMobiliarios filePath = do
  result <- parseCsvMobiliario filePath
  case result of
    Left err -> return $ Left err
    Right (_, mobiliarios) -> return $ Right mobiliarios

{- Fin: Leer csv mobiliario -------------------------------------------------------------------------------------------------}

