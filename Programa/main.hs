module Main where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.IO (putStrLn, writeFile)
import Text.Printf (printf)
import Auxiliares (User(..), getUsers)
import Funcs 
    ( Mobiliario(..)
    , Sala(..)
    , Reserva(..)
    , MobiliariosCargados
    , SalasCreadas
    , ReservasCreadas
    , getMobiliarios
    , guardarMobiliarioCSV
    , crearSala
    , mostrarInformacionSala
    , guardarSalasComoJSON
    , cargarSalasDesdeJSON
    , crearReserva
    , guardarReservasComoJSON
    , cargarReservasDesdeJSON
    , mostrarInformacionReserva
    , cancelarReserva
    , consultarDisponibilidadPorFecha
    , consultarDisponibilidadPorRango
    , modificarReserva
    )

-- Referencia mutable para almacenar el usuario actual
type UsuarioActual = IORef (Maybe User)

-- Validar ID de usuario
validarIdUsuario :: V.Vector User -> UsuarioActual -> IO (Maybe String)
validarIdUsuario usuarios usuarioRef = do
    putStrLn "\nIngrese el ID del usuario ('v' para volver al menú principal): "
    idIngresado <- getLine
    if idIngresado == "v"
        then return Nothing
        else do
            let usuarioExiste = V.find (\u -> idUsuario u == idIngresado) usuarios
            case usuarioExiste of
                Just usuario -> do
                    writeIORef usuarioRef (Just usuario)
                    return (Just idIngresado)
                Nothing -> do
                    putStrLn "\nID no encontrado. Intente de nuevo."
                    validarIdUsuario usuarios usuarioRef

-- Agregar mobiliarios
agregarMobiliarios :: MobiliariosCargados -> V.Vector Mobiliario -> IO ()
agregarMobiliarios mobiliariosRef nuevosMobiliarios = do
    mobiliariosExistentes <- readIORef mobiliariosRef
    let (mobiliariosValidos, duplicados) = V.partition (\nuevo -> not (esCodigoDuplicado nuevo mobiliariosExistentes)) nuevosMobiliarios
    
    forM_ duplicados $ \mobiliario -> 
        putStrLn $ "No se cargó el mobiliario con codigo " ++ codigo mobiliario ++ " porque ya existe en los registros"
    
    let mobiliariosActualizados = mobiliariosExistentes V.++ mobiliariosValidos
    writeIORef mobiliariosRef mobiliariosActualizados

-- Comprobar si el código está duplicado
esCodigoDuplicado :: Mobiliario -> V.Vector Mobiliario -> Bool
esCodigoDuplicado nuevoMobiliario mobiliariosExistentes =
    V.any (\mobiliario -> codigo mobiliario == codigo nuevoMobiliario) mobiliariosExistentes

-- Mostrar submenú de opciones operativas
mostrarSubmenuOO :: IO ()
mostrarSubmenuOO = do
    putStrLn " _____________________________________"
    putStrLn "|                                     |"
    putStrLn "|         Opciones Operativas         |"
    putStrLn "|_____________________________________|"
    putStrLn ""
    putStrLn "1. Cargar y Mostrar mobiliario de sala"
    putStrLn "2. Crear salas de reunión"
    putStrLn "3. Mostrar salas de reunión"
    putStrLn "4. Informe de reservas"
    putStrLn "5. Volver"
    putStrLn ""
    putStrLn "Seleccione una opción: "

-- Manejar el menú de opciones operativas
mainOO :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> IO ()
mainOO mobiliariosRef salasRef reservasRef = do
    mostrarSubmenuOO
    opcion <- getLine
    let opcionInt = read opcion :: Int
    case opcionInt of
        1 -> do
            putStrLn "\nIngrese la ruta del archivo de mobiliario: "
            filePath <- getLine
            result <- getMobiliarios filePath
            case result of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    mainOO mobiliariosRef salasRef reservasRef
                Right nuevosMobiliarios -> do
                    agregarMobiliarios mobiliariosRef nuevosMobiliarios
                    mobiliariosActualizados <- readIORef mobiliariosRef
                    putStrLn "\nMobiliario en registro:\n"
                    forM_ mobiliariosActualizados $ \mobiliario -> do
                        putStrLn $ "Código: " ++ codigo mobiliario
                        putStrLn $ "Nombre: " ++ nombre mobiliario
                        putStrLn $ "Descripción: " ++ descripcion mobiliario
                        putStrLn $ "Tipo: " ++ tipo mobiliario
                        putStrLn ""
                    mainOO mobiliariosRef salasRef reservasRef
        2 -> do
            codigo <- crearSala mobiliariosRef salasRef
            putStrLn $ "\nSala creada con el código: " ++ codigo
            mainOO mobiliariosRef salasRef reservasRef
        3 -> do
            putStrLn "\nIngrese el código de la sala o ingresa 'Todo' para ver todas las salas: "
            codigoSala <- getLine
            mostrarInformacionSala mobiliariosRef salasRef codigoSala
            mainOO mobiliariosRef salasRef reservasRef
        4 -> do
            putStrLn "Has seleccionado 4"
            mainOO mobiliariosRef salasRef reservasRef
        5 -> do      
            putStrLn "\nVolviendo al menú principal..."
            mainLoop mobiliariosRef salasRef reservasRef
        _ -> do
            putStrLn "\nOpción inválida. Vuelva a intentar."
            mainOO mobiliariosRef salasRef reservasRef

-- Mostrar submenú de opciones generales
mostrarSubmenuOG :: IO ()
mostrarSubmenuOG = do
    putStrLn " _____________________________________"
    putStrLn "|                                     |"
    putStrLn "|         Opciones Generales          |"
    putStrLn "|_____________________________________|"
    putStrLn ""
    putStrLn "1. Gestion de reserva"
    putStrLn "2. Consultar Reserva"
    putStrLn "3. Cancelar Reserva"
    putStrLn "4. Modificar Reserva"
    putStrLn "5. Consultar la disponibilidad por rango de fecha"
    putStrLn "6. Consultar la disponibilidad por fecha"
    putStrLn "7. Salir"
    putStrLn ""
    putStrLn "Seleccione una opción: "

-- Manejar el menú de opciones generales
mainOG :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> IO ()
mainOG mobiliariosRef salasRef reservasRef = do
    mostrarSubmenuOG
    opcion <- getLine
    let opcionInt = read opcion :: Int
    case opcionInt of
        1 -> do
            codigo <- crearReserva reservasRef salasRef
            putStrLn $ "\nReserva creada con código " ++ codigo
            mainOG mobiliariosRef salasRef reservasRef
        2 -> do
            putStrLn "\nIngrese el código de la reserva o ingresa 'Todo' para ver todas las reservas: "
            codigoReserva <- getLine
            mostrarInformacionReserva mobiliariosRef salasRef reservasRef codigoReserva
            mainOG mobiliariosRef salasRef reservasRef
        3 -> do
            putStrLn "\nIngrese el código de la reserva a eliminar: "
            codigoReserva <- getLine
            cancelarReserva reservasRef codigoReserva
            mainOG mobiliariosRef salasRef reservasRef
        4 -> do
            modificarReserva reservasRef salasRef
            mainOG mobiliariosRef salasRef reservasRef
        5 -> do
            putStrLn "\nConsulta disponibilidad por rango de fechas"
            putStrLn "Ingrese la fecha de inicio (YYYY-MM-DD):"
            fechaInicio <- getLine
            putStrLn "Ingrese la fecha de fin (YYYY-MM-DD):"
            fechaFin <- getLine
            consultarDisponibilidadPorRango reservasRef salasRef fechaInicio fechaFin
            mainOG mobiliariosRef salasRef reservasRef 
        6 -> do      
            putStrLn "\nConsulta disponibilidad por fecha"
            putStrLn "Ingrese la fecha para consultar disponibilidad (YYYY-MM-DD):"
            fechaConsulta <- getLine
            consultarDisponibilidadPorFecha reservasRef salasRef fechaConsulta  -- No hay necesidad de convertir a lista
            mainOG mobiliariosRef salasRef reservasRef 
        7 -> do      
            putStrLn "\nVolviendo al menú principal..."
            mainLoop mobiliariosRef salasRef reservasRef
        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            mainOG mobiliariosRef salasRef reservasRef

            
-- Mostrar menú principal
mostrarMenuPrincipal :: IO ()
mostrarMenuPrincipal = do
    putStrLn " _____________________________________"
    putStrLn "|                                     |"
    putStrLn "|   Gestión de Espacios de Trabajo    |"
    putStrLn "|_____________________________________|"
    putStrLn ""
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStrLn ""
    putStrLn "Seleccione una opción: "

-- Definición de la función mainLoop
mainLoop :: MobiliariosCargados -> SalasCreadas -> ReservasCreadas -> IO ()
mainLoop mobiliariosRef salasRef reservasRef = do
    
    usuarioRef <- newIORef Nothing

    mostrarMenuPrincipal
    opcion <- getLine
    let opcionInt = read opcion :: Int
    case opcionInt of
        1 -> do 
            result <- getUsers
            case result of
                Left err -> putStrLn $ "Error: " ++ err
                Right usuarios -> do
                    idValido <- validarIdUsuario usuarios usuarioRef
                    case idValido of
                        Nothing -> mainLoop mobiliariosRef salasRef reservasRef
                        Just _ -> do
                            usuarioActual <- readIORef usuarioRef
                            case usuarioActual of
                                Just usuario -> putStrLn $ "\nBienvenido, " ++ nombreCompleto usuario
                                Nothing -> putStrLn "\nError: Usuario no encontrado"
                            putStrLn "\nEntrando al submenú de Opciones Operativas"
                            mainOO mobiliariosRef salasRef reservasRef
        2 -> do 
            mainOG mobiliariosRef salasRef reservasRef
        3 -> do
            putStrLn "\nSaliendo del programa..."
            guardarMobiliarioCSV "Archivos del sistema/mobiliario.csv" mobiliariosRef
            guardarSalasComoJSON "Archivos del sistema/salas.json" salasRef
            guardarReservasComoJSON "Archivos del sistema/reservas.json" reservasRef
        _ -> do
            putStrLn "\nOpción inválida. Vuelva a intentar."
            mainLoop mobiliariosRef salasRef reservasRef

-- Función principal
main :: IO ()
main = do
    mobiliariosRef <- newIORef V.empty 
    salasRef <- newIORef V.empty 
    reservasRef <- newIORef V.empty 
    
    -- Verificar si el archivo de mobiliario existe
    existeMobiliario <- doesFileExist "Archivos del sistema/mobiliario.csv"
    if existeMobiliario
        then do
            result <- getMobiliarios "Archivos del sistema/mobiliario.csv"
            case result of
                Left err -> putStrLn $ "Error al cargar mobiliarios: " ++ err
                Right nuevosMobiliarios -> do
                    if V.null nuevosMobiliarios
                        then putStrLn "\nNo se encontraron mobiliarios registrados en el archivo CSV del sistema."
                        else do
                            agregarMobiliarios mobiliariosRef nuevosMobiliarios
                            mobiliariosActualizados <- readIORef mobiliariosRef
                            putStrLn "\nMobiliario en registro:\n"
                            forM_ mobiliariosActualizados $ \mobiliario -> do
                                putStrLn $ "Código: " ++ codigo mobiliario
                                putStrLn $ "Nombre: " ++ nombre mobiliario
                                putStrLn $ "Descripción: " ++ descripcion mobiliario
                                putStrLn $ "Tipo: " ++ tipo mobiliario
                                putStrLn ""
        else putStrLn "El archivo de mobiliario no existe."

    -- Cargar las salas desde el archivo JSON
    cargarSalasDesdeJSON "Archivos del sistema/salas.json" salasRef

    -- Mostrar las salas cargadas
    salasCreadas <- readIORef salasRef
    if V.null salasCreadas
        then putStrLn "No se encontraron salas registradas en el archivo JSON del sistema."
        else do
            putStrLn "\nSalas en registro:\n"
            forM_ salasCreadas $ \sala -> do
                putStrLn $ "Código de sala: " ++ codigoSala sala
                putStrLn $ "Nombre de sala: " ++ nombreSala sala
                putStrLn $ "Edificio: " ++ edificio sala
                putStrLn $ "Piso: " ++ show (piso sala)
                putStrLn $ "Capacidad: " ++ show (capacidad sala)
                putStrLn $ "Ubicación: " ++ ubicacion sala
                putStrLn $ "Mobiliario seleccionado: " ++ show (mobiliarioSeleccionado sala)
                putStrLn ""

    --Cargar las reservas
    cargarReservasDesdeJSON "Archivos del sistema/reservas.json" reservasRef
    
    -- Mostrar las reservas cargadas
    reservasCreadas <- readIORef reservasRef
    if V.null reservasCreadas
        then putStrLn "No se encontraron reservas registradas en el archivo JSON del sistema."
        else do
            putStrLn "\nReservas en registro:\n"
            forM_ reservasCreadas $ \reserva -> do
                putStrLn $ "Código de reserva: " ++ codigoReserva reserva
                putStrLn $ "ID de usuario: " ++ idUsuarioReserva reserva 
                putStrLn $ "Código de sala reservada: " ++ codigoSalaReserva reserva
                putStrLn $ "Fecha de la reserva: " ++ fecha reserva
                putStrLn $ "Cantidad de personas: " ++ show (cantidadPersonas reserva)
                putStrLn ""
                
    -- Iniciar el ciclo del menú principal
    mainLoop mobiliariosRef salasRef reservasRef

