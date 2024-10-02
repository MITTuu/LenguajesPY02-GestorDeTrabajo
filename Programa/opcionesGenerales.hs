{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OpcionesGenerales where

import Data.Aeson (decode, encode, ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL  -- Importa correctamente BL
import Data.List (find)
import Data.Maybe (isNothing)
import Control.Monad (when)

-- Definir las estructuras de datos para Reserva y Sala
data Reserva = Reserva 
    { codigoReserva :: String
    , codigoSala :: String  -- Este es el campo correcto para obtener el código de la sala en Reserva
    , fecha :: String
    , cantidadPersonas :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON Reserva
instance FromJSON Reserva

data Sala = Sala 
    { codigoSalaLocal :: String
    , capacidad :: Int
    , edificio :: String
    , mobiliarioSeleccionado :: [String]
    , nombreSala :: String
    , piso :: Int
    , ubicacion :: String
    } deriving (Show, Eq, Generic)

instance ToJSON Sala
instance FromJSON Sala

-- Función para cargar reservas desde un archivo JSON
cargarReservas :: IO (Maybe [Reserva])
cargarReservas = do
    contenido <- BL.readFile "reservas.json"  -- Usar BL.readFile
    let reservas = decode contenido :: Maybe [Reserva]
    return reservas

-- Función para cargar salas desde un archivo JSON
cargarSalas :: IO (Maybe [Sala])
cargarSalas = do
    contenido <- BL.readFile "salas.json"  -- Usar BL.readFile
    let salas = decode contenido :: Maybe [Sala]
    return salas

-- Función para guardar reservas en un archivo JSON
guardarReservas :: [Reserva] -> IO ()
guardarReservas reservas = BL.writeFile "reservas.json" (encode reservas)

-- Crear una nueva reserva después de verificar disponibilidad
crearReserva :: String -> String -> String -> Int -> IO ()
crearReserva idUsuario idSala fechaSolicitada cantidad = do
    -- Cargar reservas existentes
    maybeReservas <- cargarReservas
    let reservas = maybe [] id maybeReservas
    
    -- Verificar si la sala ya tiene una reserva en la fecha solicitada
    let reservaExistente = find (\r -> codigoSala r == idSala && fecha r == fechaSolicitada) reservas
    
    -- Cargar las salas desde el archivo
    maybeSalas <- cargarSalas
    let salas = maybe [] id maybeSalas
    
    if isNothing reservaExistente
        then do
            -- Verificar si la sala existe y tiene capacidad suficiente
            let sala = find (\s -> codigoSalaLocal s == idSala) salas
            case sala of
                Just s -> do
                    putStrLn $ "Sala encontrada: " ++ codigoSalaLocal s ++ " con capacidad: " ++ show (capacidad s)
                    if capacidad s >= cantidad
                        then do
                            -- Crear un nuevo código de reserva
                            let nuevoCodigoReserva = "R" ++ show (length reservas + 1)
                            let nuevaReserva = Reserva { codigoReserva = nuevoCodigoReserva, codigoSala = idSala, fecha = fechaSolicitada, cantidadPersonas = cantidad }
                            -- Agregar la nueva reserva y guardar en el archivo
                            let reservasActualizadas = nuevaReserva : reservas
                            guardarReservas reservasActualizadas
                            putStrLn $ "Reserva creada: " ++ show nuevaReserva
                        else putStrLn $ "La sala tiene capacidad insuficiente: " ++ show (capacidad s) ++ " para " ++ show cantidad ++ " personas."
                Nothing -> putStrLn $ "No se encontró la sala con el ID: " ++ idSala
        else putStrLn "Ya existe una reserva para esa sala en la fecha solicitada."

-- Consultar una reserva por código
consultarReserva :: String -> IO ()
consultarReserva codigo = do
    -- Cargar reservas desde el archivo JSON
    maybeReservas <- cargarReservas
    let reservas = maybe [] id maybeReservas
    -- Buscar la reserva por el código
    let reserva = find (\r -> codigoReserva r == codigo) reservas
    -- Mostrar los detalles de la reserva o mensaje si no existe
    case reserva of
        Just r -> do
            putStrLn "Reserva encontrada:"
            putStrLn $ "Código de reserva: " ++ codigoReserva r
            putStrLn $ "Código de sala: " ++ codigoSala r  -- Usar correctamente el campo codigoSala en Reserva
            putStrLn $ "Fecha de reserva: " ++ fecha r
            putStrLn $ "Cantidad de personas: " ++ show (cantidadPersonas r)
        Nothing -> putStrLn "Reserva no encontrada."

-- Cancelar una reserva por código
cancelarReserva :: String -> IO ()
cancelarReserva codigo = do
    maybeReservas <- cargarReservas
    let reservas = maybe [] id maybeReservas
    let reservasActualizadas = filter (\r -> codigoReserva r /= codigo) reservas
    guardarReservas reservasActualizadas
    putStrLn $ "Reserva con código " ++ codigo ++ " cancelada."

-- Modificar una reserva
modificarReserva :: String -> String -> String -> Int -> IO ()
modificarReserva codigo idSala nuevaFecha nuevaCantidad = do
    maybeReservas <- cargarReservas
    let reservas = maybe [] id maybeReservas
    let reservasModificadas = map (\r -> if codigoReserva r == codigo 
                                          then r { codigoSala = idSala, fecha = nuevaFecha, cantidadPersonas = nuevaCantidad } 
                                          else r) reservas
    guardarReservas reservasModificadas
    putStrLn $ "Reserva modificada a: Sala: " ++ idSala ++ ", Fecha: " ++ nuevaFecha ++ ", Cantidad: " ++ show nuevaCantidad

-- Consulta de disponibilidad de sala
consultarDisponibilidad :: String -> IO ()
consultarDisponibilidad fechaConsulta = do
    maybeSalas <- cargarSalas
    let salas = maybe [] id maybeSalas
    let salasDisponibles = filter (\s -> True) salas  -- Aquí iría la lógica de disponibilidad real
    putStrLn $ "Salas disponibles para el " ++ fechaConsulta ++ ": " ++ show salasDisponibles
