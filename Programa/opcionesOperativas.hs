module Operativas 
  ( Mobiliario(..)
  , Sala(..)
  , MobiliariosCargados
  , SalasCreadas
  , getMobiliarios
  , guardarMobiliarioCSV
  , crearSala
  , mostrarInformacionSala
  , guardarSalasComoJSON 
  , cargarSalasDesdeJSON
  ) where

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<*>), (<$>))
import Control.Exception (catch, IOException)
import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv as Csv
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (filter)
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.IO (writeFile)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude hiding (filter)
import Data.Aeson as Aeson
import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString, readFile)

-- Estructura de datos para los mobiliarios de sala
data Mobiliario = Mobiliario
  { codigo :: String
  , nombre :: String
  , descripcion :: String
  , tipo :: String
  } deriving (Show, Eq)

-- Estructura de datos para las salas de reuniones
data Sala = Sala
  { codigoSala :: String
  , nombreSala :: String
  , edificio :: String
  , piso :: Int
  , ubicacion :: String
  , capacidad :: Int
  , mobiliarioSeleccionado :: [String]
  } deriving (Show, Generic)

-- Alias para el mobiliario cargado
type MobiliariosCargados = IORef (V.Vector Mobiliario)

-- Alias para las salas creadas
type SalasCreadas = IORef (V.Vector Sala)

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
    <$> record Csv..: B8.pack "codigo"
    <*> record Csv..: B8.pack "nombre"
    <*> record Csv..: B8.pack "descripcion"
    <*> record Csv..: B8.pack "tipo"

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
      return $ Csv.decodeByName csvDataMobiliario
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

{- Inicio: Persistencia de datos para el mobiliario cargado -------------------------------------------------------------}

-- Instancia para convertir un mobiliario a un registro CSV
instance ToNamedRecord Mobiliario where
  toNamedRecord mobiliario = namedRecord
    [ B8.pack "codigo" Csv..= codigo mobiliario
    , B8.pack "nombre" Csv..= nombre mobiliario
    , B8.pack "descripcion" Csv..= descripcion mobiliario
    , B8.pack "tipo" Csv..= tipo mobiliario
    ]

-- Instancia para definir el orden de las columnas del CSV
instance DefaultOrdered Mobiliario where
  headerOrder _ = header
    [ B8.pack "codigo"
    , B8.pack "nombre"
    , B8.pack "descripcion"
    , B8.pack "tipo"
    ]

{- 
/*****Nombre****************************************
 * guardarMobiliariosCSV
 *****Descripción***********************************
 * Toma el estado actual de `MobiliariosCargados`, lo convierte 
 * a formato CSV y lo guarda en el archivo especificado.
 *****Parámetros************************************
 * @filePath: Ruta del archivo CSV donde se guardará la información.
 * @mobiliariosRef: Referencia mutable que contiene el vector de mobiliarios.
 *****Retorno***************************************
 * @IO (): Realiza la acción de guardar el archivo CSV.
 ***************************************************/
-}
guardarMobiliarioCSV :: FilePath -> MobiliariosCargados -> IO()
guardarMobiliarioCSV filePath mobiliariosRef = do
    mobiliarios <- readIORef mobiliariosRef
    
    -- Codificar los datos como CSV
    let csvData = Csv.encodeByName (headerOrder (undefined :: Mobiliario)) (V.toList mobiliarios)
    
    -- Escribir el CSV en el archivo
    catch (BL.writeFile filePath csvData >> putStrLn "Mobiliarios guardados exitosamente en el archivo CSV.")
          manejarError

{- Fin: Persistencia de datos para el mobiliario cargado -----------------------------------------------------------------}

{- Inicio: Crear y mostrar salas de reuniones -----------------------------------------------------------------------------}

{- 
/*****Nombre****************************************
 * generarCodigoSala
 *****Descripción***********************************
 * Genera un código único para una sala de reuniones,
 * que consiste en tres letras aleatorias seguidas de
 * tres números aleatorios. Este código es utilizado para
 * identificar de manera única cada sala creada en el sistema.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO String: Un código de sala único en formato de 
 * cadena, compuesto por tres letras y tres números.
 ***************************************************/
-}
generarCodigoSala :: IO String
generarCodigoSala = do
    letras <- replicateM 3 (randomRIO ('A', 'Z'))
    numeros <- replicateM 3 (randomRIO ('0', '9'))
    return $ letras ++ numeros

{- 
/*****Nombre****************************************
 * agregarSala
 *****Descripción***********************************
 * Agrega una nueva sala creada al vector de salas existentes.
 *****Parámetros************************************
 * @salasRef: Referencia mutable que contiene el vector de salas.
 * @nuevaSala: La sala recién creada que se agregará a la lista.
 *****Retorno***************************************
 * @IO (): Actualiza el vector de salas añadiendo la nueva sala.
 ***************************************************/
-}
agregarSala :: SalasCreadas -> Sala -> IO ()
agregarSala salasRef nuevaSala = do
    salas <- readIORef salasRef
    let salasActualizadas = V.snoc salas nuevaSala
    writeIORef salasRef salasActualizadas

{- 
/*****Nombre****************************************
 * crearSala
 *****Descripción***********************************
 * Solicita al usuario información para crear una sala de
 * reuniones, genera un código único y almacena la nueva sala
 * en el vector de salas cargadas.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable que contiene
 * los mobiliarios cargados hasta el momento.
 * @salasRef: Referencia mutable que contiene las salas creadas.
 *****Retorno***************************************
 * @IO Sala: La sala recién creada con todos los datos ingresados.
 ***************************************************/
-}
crearSala :: MobiliariosCargados -> SalasCreadas -> IO String
crearSala mobiliariosRef salasRef = do
    -- Generar el código de la nueva sala
    codigoSala <- generarCodigoSala

    -- Validar nombre de la sala
    nombreSala <- solicitarDatoNoVacio "\nIngrese el nombre de la sala: "

    -- Validar edificio
    edificio <- solicitarDatoNoVacio "\nIngrese el edificio: "

    -- Validar ubicación
    ubicacion <- solicitarDatoNoVacio "\nIngrese la ubicación: "

    -- Validar piso (debe ser un número entero entre 1 y 100)
    piso <- solicitarPisoValido "\nIngrese el piso (1-100): "

    -- Validar capacidad (debe ser un número entero entre 1 y 20)
    capacidad <- solicitarCapacidadValida "\nIngrese la capacidad (1-20): "

    -- Mostrar mobiliarios existentes
    putStrLn "\nMobiliario disponible:\n"
    mobiliarios <- readIORef mobiliariosRef
    forM_ (V.toList mobiliarios) $ \mobiliario -> do
        putStrLn $ "Código: " ++ codigo mobiliario ++ ", Nombre: " ++ nombre mobiliario

    -- Seleccionar y validar mobiliarios
    codigosSeleccionados <- solicitarCodigosValidos mobiliariosRef "\nIngrese los códigos de mobiliarios a añadir (separados por comas): "

    -- Crear la nueva sala
    let nuevaSala = Sala codigoSala nombreSala edificio piso ubicacion capacidad codigosSeleccionados

    -- Agregar la sala al vector de salas
    agregarSala salasRef nuevaSala

    return codigoSala

{- 
/*****Nombre****************************************
 * solicitarDatoNoVacio
 *****Descripción***********************************
 * Solicita un dato al usuario y asegura que no esté vacío.
 * Si el dato es vacío, vuelve a solicitarlo.
 *****Parámetros************************************
 * @mensaje: Mensaje que se mostrará al usuario para solicitar el dato.
 *****Retorno***************************************
 * @IO String: El dato ingresado por el usuario, garantizado que no es vacío.
 ***************************************************/
-}
solicitarDatoNoVacio :: String -> IO String
solicitarDatoNoVacio mensaje = do
    putStrLn mensaje
    input <- getLine
    if null input
    then do
        putStrLn "\nEl campo no puede estar vacío. Intente de nuevo."
        solicitarDatoNoVacio mensaje
    else return input
    
{- 
/*****Nombre****************************************
 * solicitarPisoValido
 *****Descripción***********************************
 * Solicita el piso de la sala y valida que sea un número
 * entero entre 1 y 100. Vuelve a solicitar si la validación
 * falla.
 *****Parámetros************************************
 * @mensaje: Mensaje que se mostrará al usuario para solicitar el piso.
 *****Retorno***************************************
 * @IO Int: El piso ingresado por el usuario, garantizado que está en el rango válido.
 ***************************************************/
-}
solicitarPisoValido :: String -> IO Int
solicitarPisoValido mensaje = do
    putStrLn mensaje
    input <- getLine
    let piso = readMaybe input :: Maybe Int
    case piso of
        Just n | n >= 1 && n <= 100 -> return n
        _ -> do
            putStrLn "\nEl piso debe ser un número entre 1 y 100. Intente de nuevo."
            solicitarPisoValido mensaje

{- 
/*****Nombre****************************************
 * solicitarCapacidadValida
 *****Descripción***********************************
 * Solicita la capacidad de la sala y valida que sea un número
 * entero entre 1 y 20. Vuelve a solicitar si la validación
 * falla.
 *****Parámetros************************************
 * @mensaje: Mensaje que se mostrará al usuario para solicitar la capacidad.
 *****Retorno***************************************
 * @IO Int: La capacidad ingresada por el usuario, garantizado que está en el rango válido.
 ***************************************************/
-}
solicitarCapacidadValida :: String -> IO Int
solicitarCapacidadValida mensaje = do
    putStrLn mensaje
    input <- getLine
    let capacidad = readMaybe input :: Maybe Int
    case capacidad of
        Just n | n >= 1 && n <= 20 -> return n
        _ -> do
            putStrLn "\nLa capacidad debe ser un número entero entre 1 y 20. Intente de nuevo."
            solicitarCapacidadValida mensaje

{- 
/*****Nombre****************************************
 * solicitarCodigosValidos
 *****Descripción***********************************
 * Solicita los códigos de mobiliarios a añadir a la sala 
 * y valida que para cada código ingresado exista un mobiliario que lo tenga.
 * Vuelve a solicitar si hay códigos inválidos.
 *****Parámetros************************************
 * @mobiliarios: Vector de mobiliarios cargados.
 * @mensaje: Mensaje para solicitar la entrada.
 *****Retorno***************************************
 * @IO [String]: Lista de códigos válidos ingresados por el usuario.
 ***************************************************/
-}
solicitarCodigosValidos :: MobiliariosCargados -> String -> IO [String]
solicitarCodigosValidos mobiliariosRef mensaje = do
    putStrLn mensaje
    input <- getLine
    let codigosIngresados = S.fromList . map (filter (/= ' ')) . splitOn ',' $ input
    
    mobiliarios <- readIORef mobiliariosRef
    let codigosValidos = S.fromList . V.toList $ V.map codigo mobiliarios
    let codigosInvalidos = S.toList $ S.difference codigosIngresados codigosValidos

    if null codigosInvalidos 
        then return $ S.toList codigosIngresados  
        else do
            putStrLn $ "\nCódigos inválidos: " ++ show codigosInvalidos ++ ". Intente de nuevo."
            solicitarCodigosValidos mobiliariosRef mensaje

-- Función auxiliar para dividir la cadena en función de una separación
splitOn :: Char -> String -> [String]
splitOn sep str = case dropWhile (== sep) str of
    "" -> []
    str' -> w : splitOn sep s
        where (w, s) = break (== sep) str'

{- 
/*****Nombre****************************************
 * mostrarInformacionSala
 *****Descripción***********************************
 * Muestra la información de una sala dada su código.
 * Busca en el vector de salas creadas y muestra los detalles
 * de la sala correspondiente si se encuentra.
 *****Parámetros************************************
 * @salasRef: Referencia mutable que contiene las salas creadas.
 * @codigoSala: El código de la sala cuya información se desea mostrar.
 *****Retorno***************************************
 * @IO (): Realiza la acción de mostrar la información de la sala.
 ***************************************************/
-}
mostrarInformacionSala :: MobiliariosCargados -> SalasCreadas -> String -> IO ()
mostrarInformacionSala mobiliariosRef salasRef codigoBuscado = do
    salas <- readIORef salasRef

    -- Validación para verificar si no hay salas registradas
    if V.null salas
        then putStrLn "\nNo hay salas registradas en el sistema."
        
        -- Si el codigo es Todo, se muestran todas las salas registradas
        else if codigoBuscado == "Todo"
            then do
                putStrLn "\nMostrando información de todas las salas:\n"
                forM_ (V.toList salas) $ \sala -> do
                    putStrLn $ "\nInformación de la sala:\n" ++
                        "\nCódigo: " ++ codigoSala sala ++ "\n" ++
                        "Nombre: " ++ nombreSala sala ++ "\n" ++
                        "Edificio: " ++ edificio sala ++ "\n" ++
                        "Piso: " ++ show (piso sala) ++ "\n" ++
                        "Ubicación: " ++ ubicacion sala ++ "\n" ++
                        "Capacidad: " ++ show (capacidad sala)

                    putStrLn "\nInformación de los mobiliarios asociados:"
                    forM_ (mobiliarioSeleccionado sala) $ \codigoMobiliario -> do
                        mostrarInformacionMobiliario mobiliariosRef codigoMobiliario
            -- Mostrar la sala para un codigo especifico
            else do
                let salaEncontrada = V.find (\sala -> codigoSala sala == codigoBuscado) salas
                case salaEncontrada of
                    Just sala -> do
                        putStrLn $ "\nInformación de la sala:\n" ++
                            "\nCódigo: " ++ codigoSala sala ++ "\n" ++
                            "Nombre: " ++ nombreSala sala ++ "\n" ++
                            "Edificio: " ++ edificio sala ++ "\n" ++
                            "Piso: " ++ show (piso sala) ++ "\n" ++
                            "Ubicación: " ++ ubicacion sala ++ "\n" ++
                            "Capacidad: " ++ show (capacidad sala)
                        
                        putStrLn "\nInformación de los mobiliarios asociados:"
                        forM_ (mobiliarioSeleccionado sala) $ \codigoMobiliario -> do
                            mostrarInformacionMobiliario mobiliariosRef codigoMobiliario

                    Nothing -> putStrLn $ "\nNo se encontró ninguna sala con el código: " ++ codigoBuscado

{- 
/*****Nombre****************************************
 * mostrarInformacionMobiliario
 *****Descripción***********************************
 * Dado un código de mobiliario, busca y muestra
 * la información del mobiliario correspondiente.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable que contiene los mobiliarios cargados.
 * @codigoBuscado: Código del mobiliario que se desea buscar.
 *****Retorno***************************************
 * @IO (): Muestra la información del mobiliario si se encuentra.
 ***************************************************/
-}
mostrarInformacionMobiliario :: MobiliariosCargados -> String -> IO ()
mostrarInformacionMobiliario mobiliariosRef codigoBuscado = do
    mobiliarios <- readIORef mobiliariosRef
    let mobiliarioEncontrado = V.find (\mobiliario -> codigo mobiliario == codigoBuscado) mobiliarios

    case mobiliarioEncontrado of
        Just mobiliario -> putStrLn $
            "Código: " ++ codigo mobiliario ++ "\n" ++
            "Nombre: " ++ nombre mobiliario ++ "\n" ++
            "Descripción: " ++ descripcion mobiliario ++ "\n" ++
            "Tipo: " ++ tipo mobiliario ++ "\n"

{- Fin: Crear y mostrar salas de reuniones -------------------------------------------------------------------------------}

{- Inicio: Persistencia de datos para las salas creadas ------------------------------------------------------------------}

-- Instancia JSON para salas   
instance ToJSON Sala
instance FromJSON Sala

{- 
/*****Nombre****************************************
 * guardarSalasComoJSON
 *****Descripción***********************************
 * Guarda la información de las salas en un archivo JSON.
 * Convierte el contenido de las salas almacenadas en el IORef a
 * formato JSON y lo guarda en la ubicación especificada.
 *****Parámetros************************************
 @ filePath: Ruta para guardar el contenido
 * @salasRef: Referencia mutable que contiene el vector de salas creadas.
 *****Retorno***************************************
 * @IO (): Guarda las salas en el archivo "Archivos del sistema/salas.json".
 ***************************************************/
-}
guardarSalasComoJSON :: FilePath -> SalasCreadas -> IO ()
guardarSalasComoJSON filePath salasRef = do
    salas <- readIORef salasRef
    let jsonData = Aeson.encode (V.toList salas)
    
    catch (BL.writeFile filePath jsonData >> putStrLn "Salas guardadas exitosamente.")
          manejarError

{- 
/*****Nombre****************************************
 * cargarSalasDesdeJSON
 *****Descripción***********************************
 * Carga la información de las salas desde un archivo JSON.
 * Lee el contenido del archivo especificado y convierte
 * los datos a un vector de salas, que se almacena en
 * la referencia mutable proporcionada.
 *****Parámetros************************************
 * @filePath: Ruta del archivo JSON desde donde se cargan las salas.
 * @salasRef: Referencia mutable que contiene el vector de salas creadas.
 *****Retorno***************************************
 * @IO (): Carga las salas desde el archivo "Archivos del sistema/salas.json".
 ***************************************************/
-}
cargarSalasDesdeJSON :: FilePath -> SalasCreadas -> IO ()
cargarSalasDesdeJSON filePath salasRef = 
    catch (do
        jsonData <- BL.readFile filePath
        if BL.null jsonData
            then putStrLn "\nNo se encontraron datos de salas en el archivo del sistema."
            else case Aeson.decode jsonData of
                Just salas -> do  
                    writeIORef salasRef salas
 
                Nothing -> putStrLn "\nError: No se pudo decodificar el JSON."
    ) manejarError

{- Fin: Persistencia de datos para las salas creadas ---------------------------------------------------------------------}

-- Función auxiliar para manejar errores de IO
manejarError :: IOException -> IO ()
manejarError e = putStrLn $ "\nOcurrió un error al guardar los datos: " ++ show e
