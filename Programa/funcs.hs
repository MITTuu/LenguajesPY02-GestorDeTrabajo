{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Funcs where

import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Calendar (addDays, Day)
import Data.IORef (readIORef)
import Control.Monad (forM_)
import Data.Maybe (isJust, fromJust)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Time (getCurrentTime, utctDay, Day, fromGregorian, parseTimeM)
import Data.Time.Format (defaultTimeLocale)
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
import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Ord (comparing)
import Auxiliares (User(..), getUsers)

{------------------------------------------------------------------------------------------------------------------------------}
{--------------------------------------------------- Estructuras de datos  ----------------------------------------------------}
{------------------------------------------------------------------------------------------------------------------------------}

-- Definir las estructuras de datos para Reserva y Sala
data Reserva = Reserva 
    { codigoReserva :: String
    , idUsuarioReserva :: String
    , codigoSalaReserva :: String 
    , fecha :: String
    , cantidadPersonas :: Int
    } deriving (Show, Eq, Generic)

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

-- Instancia para convertir un mobiliario
instance FromNamedRecord Mobiliario where
  parseNamedRecord record = Mobiliario
    <$> record Csv..: B8.pack "codigo"
    <*> record Csv..: B8.pack "nombre"
    <*> record Csv..: B8.pack "descripcion"
    <*> record Csv..: B8.pack "tipo"

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

-- Instancia JSON para salas   
instance ToJSON Sala
instance FromJSON Sala

-- Instancia JSON para reservas   
instance ToJSON Reserva
instance FromJSON Reserva

-- Alias para las salas creadas
type ReservasCreadas = IORef (V.Vector Reserva)
-- Alias para el mobiliario cargado
type MobiliariosCargados = IORef (V.Vector Mobiliario)
-- Alias para las salas creadas
type SalasCreadas = IORef (V.Vector Sala)
-- Referencia mutable para almacenar el usuario actual
type UsuarioActual = IORef (Maybe User)
-- Referencia a la instancia de mobiliario
type CsvDataMobiliario = (Header, V.Vector Mobiliario)
-- Mensaje de error
type ErrorMsg = String

{------------------------------------------------------------------------------------------------------------------------------}
{------------------------------------------------- Módulo Opciones Operativas -------------------------------------------------}
{------------------------------------------------------------------------------------------------------------------------------}

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

                    putStrLn "\nInformación de los mobiliarios asociados:\n"
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
{- 
/*****Nombre****************************************
 * informeReservas
 *****Descripción***********************************
 * Genera un informe de las reservas, mostrando todas
 * las reservas registradas y estadísticas relevantes.
 * Incluye la sala más utilizada, el usuario con mayor
 * número de reservas y el día con más reservas.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable que contiene el
 * vector de mobiliarios cargados.
 * @salasRef: Referencia mutable que contiene el vector
 * de salas creadas.
 * @reservasRef: Referencia mutable que contiene el
 * vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Imprime el informe en la consola.
 ***************************************************/
-}
informeReservas :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> IO ()
informeReservas mobiliariosRef salasRef reservasRef = do
    reservas <- readIORef reservasRef
    salas <- readIORef salasRef
    
    putStrLn "\n--- Informe de Reservas ---\n"
    
    -- Mostrar todas las reservas
    mostrarInformacionReserva mobiliariosRef salasRef reservasRef "Todo"
    
    putStrLn "\n--- Estadística ---\n"
    
    -- Mostrar la sala más utilizada
    mostrarSalaMasUtilizada mobiliariosRef salasRef reservasRef
    
    -- Mostrar el usuario con mayor reservas
    mostrarUsuarioConMasReservas reservasRef
    
    -- Mostrar el día con más reservas
    mostrarDiaConMasReservas reservasRef
    
{- 
/*****Nombre****************************************
 * mostrarSalaMasUtilizada
 *****Descripción***********************************
 * Muestra la sala que ha sido utilizada con más
 * frecuencia en las reservas. Cuenta las reservas
 * por código de sala y muestra los detalles de la sala
 * más utilizada.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable que contiene el
 * vector de mobiliarios cargados.
 * @salasRef: Referencia mutable que contiene el vector
 * de salas creadas.
 * @reservasRef: Referencia mutable que contiene el
 * vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Imprime en la consola la sala más utilizada
 * y sus detalles.
 ***************************************************/
-}
mostrarSalaMasUtilizada :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> IO ()
mostrarSalaMasUtilizada mobiliariosRef salasRef reservasRef = do
    reservas <- readIORef reservasRef
    salas <- readIORef salasRef

    if V.null reservas
        then putStrLn "No hay reservas registradas."
        else do
            -- Contar cuántas veces se ha utilizado cada sala
            let conteoSalas = foldr (\reserva acc -> Map.insertWith (+) (codigoSalaReserva reserva) 1 acc) Map.empty (V.toList reservas)

            -- Encontrar la sala más utilizada
            let salaMasUtilizada = fst $ maximumBy (comparing snd) (Map.toList conteoSalas)

            -- Mostrar los detalles de la sala más utilizada
            putStrLn $ "La sala con código " ++ salaMasUtilizada ++ " es la más utilizada."
            mostrarInformacionSala mobiliariosRef salasRef salaMasUtilizada

{- 
/*****Nombre****************************************
 * mostrarUsuarioConMasReservas
 *****Descripción***********************************
 * Muestra el usuario que ha realizado la mayor
 * cantidad de reservas. Cuenta las reservas por ID
 * de usuario y carga la información del usuario desde
 * el archivo CSV correspondiente.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el
 * vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Imprime en la consola el nombre del usuario
 * con más reservas y la cantidad total de reservas.
 ***************************************************/
-}
mostrarUsuarioConMasReservas :: ReservasCreadas -> IO ()
mostrarUsuarioConMasReservas reservasRef = do
    reservas <- readIORef reservasRef

    if V.null reservas
        then putStrLn "No hay reservas registradas."
        else do
            -- Contar cuántas reservas ha hecho cada usuario
            let conteoUsuarios = foldr (\reserva acc -> Map.insertWith (+) (idUsuarioReserva reserva) 1 acc) Map.empty (V.toList reservas)

            -- Encontrar el usuario con más reservas
            let usuarioConMasReservas = fst $ maximumBy (comparing snd) (Map.toList conteoUsuarios)

            -- Cargar los usuarios desde el archivo CSV
            usuarioRef <- newIORef Nothing 
            result <- getUsers
            case result of
                Left err -> putStrLn $ "Error al cargar usuarios: " ++ err
                Right usuarios -> do
                    -- Buscar el nombre completo del usuario con más reservas
                    let usuario = V.find (\u -> idUsuario u == usuarioConMasReservas) usuarios
                    case usuario of
                        Just u -> do
                            writeIORef usuarioRef (Just u)
                            putStrLn $ "El usuario con más reservas es " ++ nombreCompleto u ++
                                       " con " ++ show (conteoUsuarios Map.! usuarioConMasReservas) ++ " reservas."
                        Nothing -> putStrLn $ "No se encontró el usuario con ID: " ++ usuarioConMasReservas

{- 
/*****Nombre****************************************
 * mostrarDiaConMasReservas
 *****Descripción***********************************
 * Muestra el día en el que se han registrado la
 * mayor cantidad de reservas. Cuenta las reservas por
 * fecha y muestra el resultado en la consola.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el
 * vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Imprime en la consola el día con más
 * reservas y la cantidad total de reservas en ese día.
 ***************************************************/
-}
mostrarDiaConMasReservas :: ReservasCreadas -> IO ()
mostrarDiaConMasReservas reservasRef = do
    reservas <- readIORef reservasRef

    if V.null reservas
        then putStrLn "No hay reservas registradas."
        else do
            -- Contar cuántas reservas se han hecho en cada fecha
            let conteoPorFecha = foldr (\reserva acc -> Map.insertWith (+) (fecha reserva) 1 acc) Map.empty (V.toList reservas)

            -- Encontrar el día con más reservas
            let diaConMasReservas = fst $ maximumBy (comparing snd) (Map.toList conteoPorFecha)

            -- Mostrar el resultado
            putStrLn $ "\nEl día " ++ diaConMasReservas ++ " cuenta con la mayor cantidad de reservas registradas" ++
                       ", con " ++ show (conteoPorFecha Map.! diaConMasReservas) ++ " reservas."

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

-- Función auxiliar para manejar errores de IO
manejarError :: IOException -> IO ()
manejarError e = putStrLn $ "\nOcurrió un error al guardar los datos: " ++ show e

{------------------------------------------------------------------------------------------------------------------------------}
{------------------------------------------------- Módulo Opciones Generales --------------------------------------------------}
{------------------------------------------------------------------------------------------------------------------------------}

-- Validar ID de usuario
{-
/*****Nombre****************************************
 * validarIdUsuario
 *****Descripción***********************************
 * Solicita al usuario que ingrese un ID y verifica si
 * ese ID corresponde a un usuario existente en la lista
 * proporcionada. Si el ID es válido, se guarda el
 * usuario en una referencia mutable y se retorna el ID
 * ingresado. Si no, se solicita nuevamente hasta que
 * se ingrese un ID válido.
 *****Parámetros************************************
 * @usuarios: Vector que contiene los usuarios registrados.
 * @usuarioRef: Referencia mutable donde se almacenará
 * el usuario correspondiente al ID ingresado.
 *****Retorno***************************************
 * @IO (Maybe String): Un valor `Maybe` que puede ser `Just`
 * con el ID ingresado si es válido, o `Nothing` si no.
 ***************************************************/
-}
validarIdUsuario :: V.Vector User -> UsuarioActual -> IO (Maybe String)
validarIdUsuario usuarios usuarioRef = do
    putStrLn "\nIngrese el ID del usuario: "
    idIngresado <- getLine
    let usuarioExiste = V.find (\u -> idUsuario u == idIngresado) usuarios
    case usuarioExiste of
        Just usuario -> do
            writeIORef usuarioRef (Just usuario)
            return (Just idIngresado)
        Nothing -> do
            putStrLn "\nID no encontrado. Intente de nuevo."
            validarIdUsuario usuarios usuarioRef

-- Validar formato de fecha y que no sea menor a la fecha actual
{-
/*****Nombre****************************************
 * validarFecha
 *****Descripción***********************************
 * Valida si una fecha proporcionada en formato
 * "YYYY-MM-DD" es correcta y está en el futuro
 * respecto a la fecha actual. Si la fecha es válida,
 * retorna la fecha parseada; de lo contrario, retorna
 * Nothing.
 *****Parámetros************************************
 * @fechaStr: La cadena que contiene la fecha a validar.
 *****Retorno***************************************
 * @IO (Maybe Day): Un valor `Maybe` que puede ser `Just`
 * con la fecha parseada si es válida o `Nothing` si no.
 ***************************************************/
-}
validarFecha :: String -> IO (Maybe Day)
validarFecha fechaStr = do
    -- Obtener la fecha actual
    hoy <- utctDay <$> getCurrentTime
    -- Intentar parsear la fecha
    let fechaParseada = parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr :: Maybe Day

    -- Validar si la fecha está en el formato correcto y es mayor o igual a hoy
    if isJust fechaParseada && (fromJust fechaParseada) >= hoy
        then return fechaParseada
        else return Nothing

-- Función principal para crear una reserva
{-
/*****Nombre****************************************
 * crearReserva
 *****Descripción***********************************
 * Crea una nueva reserva solicitando al usuario la
 * información necesaria, como el ID del usuario,
 * el ID de la sala, la fecha de la reserva y la
 * cantidad de personas. Valida la información ingresada
 * y actualiza la lista de reservas si todo es correcto.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el
 * vector de reservas creadas.
 * @salasRef: Referencia mutable que contiene el vector
 * de salas disponibles.
 *****Retorno***************************************
 * @IO String: El código de la nueva reserva creada.
 ***************************************************/
-}
crearReserva :: ReservasCreadas -> SalasCreadas -> IO String
crearReserva reservasRef salasRef = do
    -- Leer las reservas y salas actuales
    reservas <- readIORef reservasRef
    salas <- readIORef salasRef

    -- Leer los usuarios
    resultUsuarios <- getUsers  -- Obtener los usuarios
    case resultUsuarios of
        Left errorMsg -> do
            putStrLn $ "Error al obtener los usuarios: " ++ errorMsg
            return ""  -- o manejar el error de otra manera
        Right usuarios -> do
            usuarioRef <- newIORef Nothing 

            -- Solicitar ID de usuario
            idUser <- solicitarIDUsuario usuarios usuarioRef

            -- Solicitar ID de la sala y validarlo
            idSala <- solicitarIDSala salas "\nIngrese el ID de la sala: "

            -- Solicitar fecha de la reserva y validar
            let solicitarFechaConValidacion mensaje = do
                    putStrLn mensaje
                    fechaSolicitada <- getLine
                    resultado <- validarFecha fechaSolicitada
                    case resultado of
                        Just fecha -> return fechaSolicitada  -- Retorna la fecha como String
                        Nothing -> do
                            putStrLn "Fecha inválida o menor a la fecha actual. Intente nuevamente."
                            solicitarFechaConValidacion mensaje

            fechaReserva <- solicitarFechaConValidacion "\nIngrese la fecha de la reserva (YYYY-MM-DD): "

            -- Solicitar cantidad de personas y validarla con la capacidad de la sala
            cantidad <- solicitarCantidadPersonas "\nIngrese la cantidad de personas: " idSala salas

            -- Crear un nuevo código de reserva
            let nuevoCodigoReserva = "R" ++ show (V.length reservas + 1)
            let nuevaReserva = Reserva 
                    { idUsuarioReserva = idUser
                    , codigoReserva = nuevoCodigoReserva
                    , codigoSalaReserva = idSala
                    , fecha = fechaReserva
                    , cantidadPersonas = cantidad
                    }

            -- Actualizar reservas y guardar
            let reservasActualizadas = V.snoc reservas nuevaReserva
            writeIORef reservasRef reservasActualizadas
            
            putStrLn "Reserva creada con éxito."
            return nuevoCodigoReserva



-- Función para solicitar ID de usuario y validar que exista
{-
/*****Nombre****************************************
 * solicitarIDUsuario
 *****Descripción***********************************
 * Solicita al usuario un ID de usuario y valida que
 * exista en el vector de usuarios. Si el ID es válido,
 * se retorna; de lo contrario, se genera un error.
 *****Parámetros************************************
 * @usuarios: Vector que contiene los usuarios existentes.
 * @usuarioRef: Referencia mutable que almacena el usuario
 * actual seleccionado.
 *****Retorno***************************************
 * @IO String: El ID de usuario ingresado por el usuario,
 * garantizado que corresponde a un usuario existente.
 ***************************************************/
-}
solicitarIDUsuario :: V.Vector User -> UsuarioActual -> IO String
solicitarIDUsuario usuarios usuarioRef = do
    maybeId <- validarIdUsuario usuarios usuarioRef
    case maybeId of
        Just id -> return id
        Nothing -> error ""

--Función para validar el ID de la sala
{-
/*****Nombre****************************************
 * solicitarIDSala
 *****Descripción***********************************
 * Solicita al usuario el ID de una sala y valida que
 * el ID ingresado exista en el vector de salas. Si el
 * ID no es válido, se vuelve a solicitar la entrada.
 *****Parámetros************************************
 * @salas: Vector que contiene las salas creadas.
 * @mensaje: Mensaje que se mostrará al usuario para
 * solicitar el ID de la sala.
 *****Retorno***************************************
 * @IO String: El ID de sala ingresado por el usuario,
 * garantizado que corresponde a una sala existente.
 ***************************************************/
-}
solicitarIDSala :: V.Vector Sala -> String -> IO String
solicitarIDSala salas mensaje = do
    putStrLn mensaje
    input <- getLine
    -- Validar entrada: Existencia de la sala
    if any (\s -> codigoSala s == input) (V.toList salas)  -- Verifica si el codigoSala coincide
        then return input  -- Si existe, devolvemos el ID ingresado
        else do
            putStrLn "El ID de sala ingresado no existe. Intente nuevamente."
            solicitarIDSala salas mensaje  -- Volver a solicitar el ID

-- Función para solicitar la cantidad de personas y validarla
{-
/*****Nombre****************************************
 * solicitarCantidadPersonas
 *****Descripción***********************************
 * Solicita al usuario la cantidad de personas que
 * asistirán a la reserva, asegurándose de que la
 * cantidad ingresada no exceda la capacidad de la sala
 * correspondiente. Si la entrada es inválida o si
 * la cantidad excede la capacidad de la sala, se vuelve
 * a solicitar la entrada.
 *****Parámetros************************************
 * @mensaje: Mensaje que se mostrará al usuario para
 * solicitar la cantidad de personas.
 * @idSala: El identificador de la sala a la que se
 * le está solicitando la capacidad.
 * @salasRef: Referencia mutable que contiene el vector
 * de salas creadas.
 *****Retorno***************************************
 * @IO Int: La cantidad de personas ingresada por el usuario,
 * garantizada que está dentro del rango permitido por
 * la capacidad de la sala.
 ***************************************************/
-}
solicitarCantidadPersonas :: String -> String -> V.Vector Sala -> IO Int
solicitarCantidadPersonas mensaje idSala salasRef = do
    putStrLn mensaje
    input <- getLine
    let cantidadIngresada = readMaybe input :: Maybe Int
    case cantidadIngresada of
        Just n -> do
            -- Buscar la sala correspondiente y verificar la capacidad
            let salaEncontrada = V.find (\s -> codigoSala s == idSala) salasRef  -- No convertir a lista
            case salaEncontrada of
                Just sala -> 
                    if n <= capacidad sala  -- Validar que no exceda la capacidad
                        then return n
                        else do
                            putStrLn $ "La cantidad de personas excede la capacidad de la sala (" ++ show (capacidad sala) ++ "). Intente nuevamente."
                            solicitarCantidadPersonas mensaje idSala salasRef
                Nothing -> do
                    putStrLn "Sala no encontrada. Intente nuevamente."
                    solicitarCantidadPersonas mensaje idSala salasRef
        Nothing -> do
            putStrLn "Entrada inválida. Por favor, ingrese un número entero."
            solicitarCantidadPersonas mensaje idSala salasRef

-- Consultar una reserva por código
{-
/*****Nombre****************************************
 * mostrarInformacionReserva
 *****Descripción***********************************
 * Muestra la información de una reserva específica
 * dada su código. Si el código ingresado es "Todo",
 * muestra información de todas las reservas. Además,
 * se proporciona información sobre la sala asociada
 * a cada reserva.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable que contiene el vector 
 * de mobiliarios.
 * @salasRef: Referencia mutable que contiene el vector 
 * de salas creadas.
 * @reservasRef: Referencia mutable que contiene el vector 
 * de reservas creadas.
 * @codigoReservaBuscado: El código de la reserva que se desea mostrar.
 *****Retorno***************************************
 * @IO (): Muestra la información de las reservas en la consola.
 ***************************************************/
-}
mostrarInformacionReserva :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> String -> IO ()
mostrarInformacionReserva mobiliariosRef salasRef reservasRef codigoReservaBuscado = do
    reservas <- readIORef reservasRef

    if codigoReservaBuscado == "Todo" then
        do
            -- Mostrar información de todas las reservas
            putStrLn "\nMostrando información de todas las reservas:\n"
            forM_ (V.toList reservas) $ \reserva -> do
                putStrLn $ "\nInformación de la reserva:\n" ++
                    "\nCódigo de Reserva: " ++ codigoReserva reserva ++ "\n" ++
                    "ID de usuario: " ++ idUsuarioReserva reserva ++ "\n" ++
                    "Código de Sala: " ++ codigoSalaReserva reserva ++ "\n" ++
                    "Fecha: " ++ fecha reserva ++ "\n" ++
                    "Cantidad de Personas: " ++ show (cantidadPersonas reserva)

                -- Mostrar la información de la sala asociada
                mostrarInformacionSala mobiliariosRef salasRef (codigoSalaReserva reserva)

    else do
        -- Buscar la reserva específica por su código
        let reservaEncontrada = V.find (\reserva -> codigoReserva reserva == codigoReservaBuscado) reservas

        case reservaEncontrada of
            Just reserva -> do
                -- Mostrar la información de la reserva
                putStrLn $ "\nInformación de la reserva:\n" ++
                    "\nCódigo de Reserva: " ++ codigoReserva reserva ++ "\n" ++
                    "ID de usuario: " ++ idUsuarioReserva reserva ++ "\n" ++
                    "Código de Sala: " ++ codigoSalaReserva reserva ++ "\n" ++
                    "Fecha: " ++ fecha reserva ++ "\n" ++
                    "Cantidad de Personas: " ++ show (cantidadPersonas reserva)

                -- Mostrar la información de la sala asociada
                mostrarInformacionSala mobiliariosRef salasRef (codigoSalaReserva reserva)

            Nothing -> putStrLn $ "\nNo se encontró ninguna reserva con el código: " ++ codigoReservaBuscado

{-
/*****Nombre****************************************
 * cancelarReserva
 *****Descripción***********************************
 * Permite al usuario cancelar una reserva existente
 * especificando el código de reserva. Se verifica
 * si la reserva existe y, de ser así, se elimina
 * de la lista de reservas. Se informa al usuario
 * si la cancelación fue exitosa o si no se encontró
 * la reserva.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el vector 
 * de reservas.
 * @codigo: El código de la reserva que se desea cancelar.
 *****Retorno***************************************
 * @IO (): Realiza la acción de cancelar la reserva 
 * especificada.
 ***************************************************/
-}
cancelarReserva :: ReservasCreadas -> String -> IO ()
cancelarReserva reservasRef codigo = do
    -- Cargar reservas existentes desde la referencia
    reservas <- readIORef reservasRef

    -- Verificar si la reserva existe
    let reservaExistente = V.find (\r -> codigoReserva r == codigo) reservas

    case reservaExistente of
        Just _ -> do
            -- Eliminar la reserva de la lista
            let reservasActualizadas = V.filter (\r -> codigoReserva r /= codigo) reservas
            
            -- Actualizar la referencia de reservas
            writeIORef reservasRef reservasActualizadas
            
            putStrLn $ "Reserva con código " ++ codigo ++ " ha sido cancelada."
        Nothing -> putStrLn $ "No se encontró ninguna reserva con el código " ++ codigo
        
        
{------------------------------------------------------------------------------------------------------------------------------}
-- modificar
{------------------------------------------------------------------------------------------------------------------------------}
{-
/*****Nombre****************************************
 * modificarReserva
 *****Descripción***********************************
 * Permite al usuario modificar una reserva existente.
 * Se solicita el código de reserva a modificar y se
 * presenta la información actual de la reserva.
 * El usuario puede actualizar el ID de la sala, la
 * fecha y la cantidad de personas. Se realizan
 * validaciones para asegurar que los nuevos datos
 * sean válidos antes de confirmar la modificación.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el vector 
 * de reservas.
 * @salasRef: Referencia mutable que contiene el vector 
 * de salas.
 *****Retorno***************************************
 * @IO (): Realiza la acción de modificar la reserva 
 * especificada.
 ***************************************************/
-}

modificarReserva :: ReservasCreadas -> SalasCreadas -> IO ()
modificarReserva reservasRef salasRef = do
    -- Leer las reservas y salas actuales
    reservas <- readIORef reservasRef
    salas <- readIORef salasRef

    -- Solicitar el código de reserva a modificar
    putStrLn "Ingrese el código de reserva a modificar:"
    codigoReservaInput <- getLine

    -- Buscar la reserva correspondiente
    let reservaExistente = V.find (\r -> codigoReserva r == codigoReservaInput) reservas
    
    case reservaExistente of
        Just reserva -> do
            -- Mostrar datos actuales de la reserva
            putStrLn $ "Reserva encontrada: " ++ show reserva
            
            -- Solicitar ID de la sala y validarlo
            idSalaNuevo <- solicitarIDSala salas "\nIngrese el nuevo ID de la sala (dejar vacío para no modificar): "
            
            -- Solicitar fecha de la reserva y validar
            let solicitarFechaConValidacion mensaje = do
                    putStrLn mensaje
                    fechaSolicitada <- getLine
                    resultado <- validarFecha fechaSolicitada
                    case resultado of
                        Just fecha -> return fechaSolicitada  -- Retorna la fecha como String
                        Nothing -> do
                            putStrLn "Fecha inválida o menor a la fecha actual. Intente nuevamente."
                            solicitarFechaConValidacion mensaje

            fechaReserva <- solicitarFechaConValidacion "\nIngrese la nueva fecha de la reserva (YYYY-MM-DD, dejar vacío para no modificar): "

            -- Solicitar cantidad de personas y validarla
            cantidadNueva <- solicitarCantidadPersonas "\nIngrese la nueva cantidad de personas (dejar vacío para no modificar): " (codigoSalaReserva reserva) salas

            -- Crear la nueva reserva modificada
            let nuevaReserva = reserva {
                codigoSalaReserva = if idSalaNuevo == "" then codigoSalaReserva reserva else idSalaNuevo,
                fecha = if fechaReserva == "" then fecha reserva else fechaReserva,
                cantidadPersonas = cantidadNueva
            }

            -- Validar la nueva reserva
            fechaValida <- validarFecha (fecha nuevaReserva)
            let esFechaValida = isJust fechaValida

            let salaEncontrada = V.find (\s -> codigoSala s == codigoSalaReserva nuevaReserva) salas

            case salaEncontrada of
                Just sala -> 
                    if esFechaValida && cantidadPersonas nuevaReserva <= capacidad sala
                        then do
                            -- Actualizar la reserva
                            let reservasActualizadas = V.map (\r -> if codigoReserva r == codigoReservaInput then nuevaReserva else r) reservas
                            writeIORef reservasRef reservasActualizadas
                            putStrLn "Reserva modificada con éxito."
                        else
                            putStrLn "No se pudo modificar la reserva. Verifique los datos ingresados."
                Nothing -> putStrLn "No se encontró la sala asociada a la reserva."
        
        Nothing -> putStrLn "No se encontró una reserva con ese código."





{------------------------------------------------------------------------------------------------------------------------------}
--consultar disponibilidad
{------------------------------------------------------------------------------------------------------------------------------}
{-
/*****Nombre****************************************
 * consultarDisponibilidadPorFecha
 *****Descripción***********************************
 * Consulta la disponibilidad de salas para una fecha 
 * específica. Verifica si la fecha es válida y, 
 * en caso afirmativo, lista las salas reservadas y 
 * disponibles para esa fecha.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el vector 
 * de reservas.
 * @salasRef: Referencia mutable que contiene el vector 
 * de salas.
 * @fechaConsulta: Cadena que representa la fecha para la 
 * cual se quiere consultar la disponibilidad (formato 
 * YYYY-MM-DD).
 *****Retorno***************************************
 * @IO (): Realiza la acción de mostrar la disponibilidad 
 * de salas para la fecha especificada.
 ***************************************************/
-}

consultarDisponibilidadPorFecha :: IORef (V.Vector Reserva) -> IORef (V.Vector Sala) -> String -> IO ()
consultarDisponibilidadPorFecha reservasRef salasRef fechaConsulta = do
    -- Validar el formato y valor de la fecha
    fechaValida <- validarFecha fechaConsulta

    case fechaValida of
        Nothing -> putStrLn "Fecha inválida. Asegúrate de usar el formato YYYY-MM-DD y que sea una fecha futura."
        Just _ -> do
            -- Leer reservas de memoria
            reservasMemoria <- readIORef reservasRef

            -- Cargar reservas desde el archivo JSON
            cargarReservasDesdeJSON "reservas.json" reservasRef  -- Asegúrate de que el archivo esté en la ruta correcta
            reservasArchivo <- readIORef reservasRef  -- Leer las reservas actualizadas después de cargar

            -- Leer salas
            salas <- readIORef salasRef  -- Asegúrate de que estás leyendo las salas aquí

            -- Filtrar reservas por la fecha consultada
            let reservasParaFecha = filter (\r -> fecha r == fechaConsulta) 
                                           (V.toList reservasMemoria ++ V.toList reservasArchivo)  -- Convertir a listas

            putStrLn $ "Disponibilidad de salas para la fecha: " ++ fechaConsulta
            forM_ salas $ \sala -> do
                let salaReservada = any (\r -> codigoSalaReserva r == codigoSala sala) reservasParaFecha
                if salaReservada
                    then putStrLn $ "Sala: " ++ nombreSala sala ++ " (Reservada)"
                    else putStrLn $ "Sala: " ++ nombreSala sala ++ " (Disponible)"

{------------------------------------------------------------------------------------------------------------------------------}
--consultar disponibilidad por rango
{------------------------------------------------------------------------------------------------------------------------------}
{-
/*****Nombre****************************************
 * parseDate
 *****Descripción***********************************
 * Intenta parsear una cadena de texto que representa una fecha 
 * en el formato YYYY-MM-DD a un valor de tipo Day. 
 * Si el formato es correcto, devuelve Just Day; 
 * de lo contrario, devuelve Nothing.
 *****Parámetros************************************
 * @dateStr: Una cadena de texto que representa la fecha en el 
 * formato YYYY-MM-DD.
 *****Retorno***************************************
 * @Maybe Day: Un valor que puede contener una fecha válida 
 * o Nothing si la cadena no se pudo parsear correctamente.
 ***************************************************/
-}
parseDate :: String -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe Day

{-
/*****Nombre****************************************
 * consultarDisponibilidadPorRango
 *****Descripción***********************************
 * Consulta la disponibilidad de salas en un rango de fechas 
 * especificado por el usuario. Para cada día en el rango, 
 * se verificará la disponibilidad de las salas.
 *****Parámetros************************************
 * @reservasRef: Referencia mutable que contiene el vector 
 * de reservas.
 * @salasRef: Referencia mutable que contiene el vector 
 * de salas.
 * @fechaInicioStr: Cadena que representa la fecha de inicio 
 * del rango (formato YYYY-MM-DD).
 * @fechaFinStr: Cadena que representa la fecha de fin 
 * del rango (formato YYYY-MM-DD).
 *****Retorno***************************************
 * @IO (): Realiza la acción de consultar la disponibilidad 
 * para cada día en el rango de fechas especificado.
 ***************************************************/
-}
consultarDisponibilidadPorRango :: IORef (V.Vector Reserva) -> IORef (V.Vector Sala) -> String -> String -> IO ()
consultarDisponibilidadPorRango reservasRef salasRef fechaInicioStr fechaFinStr = do
    let maybeFechaInicio = parseDate fechaInicioStr
    let maybeFechaFin = parseDate fechaFinStr

    case (maybeFechaInicio, maybeFechaFin) of
        (Just fechaInicio, Just fechaFin) -> do
            let diasRango = [fechaInicio .. fechaFin]
            mapM_ (consultarDisponibilidadPorFecha reservasRef salasRef . show) diasRango
        _ -> putStrLn "Las fechas ingresadas no son válidas. Asegúrese de utilizar el formato YYYY-MM-DD."

{-
/*****Nombre****************************************
 * guardarReservasComoJSON
 *****Descripción***********************************
 * Guarda la información de las reservas en un archivo JSON.
 * Convierte el contenido de las reservas almacenadas en el IORef a
 * formato JSON y lo guarda en la ubicación especificada.

 *****Parámetros************************************
 * @filePath: Ruta para guardar el contenido.
 * @reservasRef: Referencia mutable que contiene el vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Guarda las reservas en el archivo especificado.
 ***************************************************/
-}
guardarReservasComoJSON :: FilePath -> ReservasCreadas -> IO ()
guardarReservasComoJSON filePath reservasRef = do
    reservas <- readIORef reservasRef

    let jsonData = Aeson.encode (V.toList reservas)
    
    catch (BL.writeFile filePath jsonData >> putStrLn "Reservas guardadas exitosamente.")
          manejarError

{- 
/*****Nombre****************************************
 * cargarReservasDesdeJSON
 *****Descripción***********************************

 * Carga la información de las reservas desde un archivo JSON.

 * Lee el contenido del archivo especificado y convierte

 * los datos a un vector de reservas, que se almacena en
 * la referencia mutable proporcionada.

 *****Parámetros************************************
 * @filePath: Ruta del archivo JSON desde donde se cargan las reservas.
 * @reservasRef: Referencia mutable que contiene el vector de reservas creadas.
 *****Retorno***************************************
 * @IO (): Carga las reservas desde el archivo especificado.
 ***************************************************/
-}
cargarReservasDesdeJSON :: FilePath -> ReservasCreadas -> IO ()
cargarReservasDesdeJSON filePath reservasRef = 
    catch (do
        jsonData <- BL.readFile filePath
        if BL.null jsonData
            then putStrLn "\nNo se encontraron datos de reservas en el archivo."
            else case Aeson.decode jsonData of
                Just reservas -> do  
                    writeIORef reservasRef (V.fromList reservas)
                    putStrLn "Reservas cargadas exitosamente."
 
                Nothing -> putStrLn "\nError: No se pudo decodificar el JSON."
    ) manejarError
