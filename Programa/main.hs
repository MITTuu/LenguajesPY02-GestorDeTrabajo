module Main where

import Auxiliares (User(..), getUsers, Mobiliario(..), getMobiliarios)
import qualified Data.Vector as V
import Data.Maybe (isJust)
import System.IO (putStrLn)
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Referencia mutable para almacenar el usuario actual
type UsuarioActual = IORef (Maybe User)

-- Referencia mutable para almacenar el mobiliario cargado
type MobiliariosCargados = IORef (V.Vector Mobiliario)

{- 
/*****Nombre****************************************
 * validarIdUsuario
 *****Descripción***********************************
 * Solicita al usuario un ID y verifica si existe en la lista
 * de usuarios obtenidos del archivo CSV. Repite la solicitud
 * hasta que el ID sea válido. Almacena el usuario en una 
 * referencia global si el ID es válido.
 *****Parámetros************************************
 * @usuarios: Vector de usuarios obtenidos del archivo CSV.
 * @usuarioRef: Referencia mutable para almacenar el usuario.
 *****Retorno***************************************
 * @IO String: El ID del usuario que existe en el CSV.
 ***************************************************/
-}
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

{- 
/*****Nombre****************************************
 * agregarMobiliarios
 *****Descripción***********************************
 * Concatenar los nuevos mobiliarios a los existentes, evitando agregar 
 * mobiliarios con códigos duplicados. Si un mobiliario tiene un código 
 * que ya existe, no se carga y se muestra un mensaje de advertencia.
 *****Parámetros************************************
 * @mobiliariosRef: Referencia mutable de los mobiliarios cargados hasta el momento.
 * @nuevosMobiliarios: Vector con los nuevos mobiliarios que se desean agregar.
 *****Retorno***************************************
 * @IO (): Actualiza la referencia mutable con los mobiliarios válidos (no duplicados),
 * y muestra mensajes de advertencia si se encuentran duplicados.
 ***************************************************/
-}
agregarMobiliarios :: MobiliariosCargados -> V.Vector Mobiliario -> IO ()
agregarMobiliarios mobiliariosRef nuevosMobiliarios = do
    mobiliariosExistentes <- readIORef mobiliariosRef
    let (mobiliariosValidos, duplicados) = V.partition (\nuevo -> not (esCodigoDuplicado nuevo mobiliariosExistentes)) nuevosMobiliarios
    
    -- Mostrar advertencias por los códigos duplicados
    forM_ duplicados $ \mobiliario -> 
        putStrLn $ "No se cargó el mobiliario con codigo " ++ codigo mobiliario ++ " porque ya existe en los registros"
    
    -- Concatenar los nuevos mobiliarios válidos a los existentes
    let mobiliariosActualizados = mobiliariosExistentes V.++ mobiliariosValidos
    writeIORef mobiliariosRef mobiliariosActualizados

{- 
/*****Nombre****************************************
 * esCodigoDuplicado
 *****Descripción***********************************
 * Verifica si un mobiliario tiene un código duplicado en un vector 
 * de mobiliarios ya existentes.
 *****Parámetros************************************
 * @nuevoMobiliario: Mobiliario que se desea comprobar.
 * @mobiliariosExistentes: Vector con los mobiliarios ya cargados.
 *****Retorno***************************************
 * @Bool: Retorna `True` si el código del mobiliario ya existe en los 
 * registros, de lo contrario, retorna `False`.
 ***************************************************/
-}
esCodigoDuplicado :: Mobiliario -> V.Vector Mobiliario -> Bool
esCodigoDuplicado nuevoMobiliario mobiliariosExistentes =
    V.any (\mobiliario -> codigo mobiliario == codigo nuevoMobiliario) mobiliariosExistentes

{-
/*****Nombre****************************************
 * mostrarSubmenuOO
 *****Descripción***********************************
 * Imprime el menú de opciones operativas
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (): Realiza la acción de imprimir el menú en la consola.
 ***************************************************/
 -}
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

{-
/*****Nombre****************************************
 * mainOO -> (OO = Opciones Operativas)
 *****Descripción***********************************
 * Función para manejar el menú OO.
 * Muestra el menú al usuario, lee su entrada, procesa la opción 
 * seleccionada y repite el ciclo hasta que el usuario elija salir.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (): Controla la interacción con el usuario, imprime el menú, 
 * lee opciones, y gestiona la lógica de repetición o salida del programa.
 ***************************************************/
-}
mainOO :: MobiliariosCargados -> IO ()
mainOO mobiliariosRef = do
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
                    mainOO mobiliariosRef
                Right nuevosMobiliarios -> do
                    -- Agregar los nuevos mobiliarios a los existentes
                    agregarMobiliarios mobiliariosRef nuevosMobiliarios
                    -- Mostrar los mobiliarios actualizados
                    mobiliariosActualizados <- readIORef mobiliariosRef
                    putStrLn "\nMobiliario cargado exitosamente:"
                    forM_ mobiliariosActualizados $ \mobiliario -> do
                        putStrLn $ "Código: " ++ codigo mobiliario
                        putStrLn $ "Nombre: " ++ nombre mobiliario
                        putStrLn $ "Descripción: " ++ descripcion mobiliario
                        putStrLn $ "Tipo: " ++ tipo mobiliario
                        putStrLn ""
                    mainOO mobiliariosRef
        2 -> do
            putStrLn "Has seleccionado 2"
            mainOO mobiliariosRef
        3 -> do
            putStrLn "Has seleccionado 3"
            mainOO mobiliariosRef
        4 -> do
            putStrLn "Has seleccionado 4"
            mainOO mobiliariosRef
        5 -> do
            putStrLn "\nVolviendo al menú principal"
            main
        _ -> do
            putStrLn "\nOpción inválida. Vuelva a intentar."
            mainOO mobiliariosRef
            
{-
/*****Nombre****************************************
 * mostrarMenuPrincipal
 *****Descripción***********************************
 * Imprime el menú principal de la aplicación con opciones
 * para que el usuario seleccione una acción.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (): Realiza la acción de imprimir el menú en la consola.
 ***************************************************/
 -}
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

{-
/*****Nombre****************************************
 * main
 *****Descripción***********************************
 * Función principal del programa que ejecuta el ciclo del menú.
 * Muestra el menú al usuario, lee su entrada, procesa la opción 
 * seleccionada y repite el ciclo hasta que el usuario elija salir.
 *****Parámetros************************************
 * Ninguno.
 *****Retorno***************************************
 * @IO (): Controla la interacción con el usuario, imprime el menú, 
 * lee opciones, y gestiona la lógica de repetición o salida del programa.
 ***************************************************/
-}
main :: IO()
main = do
    usuarioRef <- newIORef Nothing
    mobiliariosRef <- newIORef V.empty 
    
    mostrarMenuPrincipal
    opcion <- getLine
    let opcionInt = read opcion :: Int
    case opcionInt of
        1 -> do 
            -- Obtener usuarios y validar ID
            result <- getUsers
            case result of
                Left err -> putStrLn $ "Error: " ++ err
                Right usuarios -> do
                    idValido <- validarIdUsuario usuarios usuarioRef
                    case idValido of
                        Nothing -> main 
                        Just _ -> do
                            usuarioActual <- readIORef usuarioRef
                            case usuarioActual of
                                Just usuario -> putStrLn $ "\nBienvenido, " ++ nombreCompleto usuario
                                Nothing -> putStrLn "\nError: Usuario no encontrado"
                            putStrLn "\nEntrando al submenú de Opciones Operativas"
                            mainOO mobiliariosRef
        2 -> do 
            putStrLn "Has seleccionado 2"
            main
        3 -> do
            putStrLn "\nSaliendo del programa..."
        _ -> do
            putStrLn "\nOpción inválida. Vuelva a intentar."
            main
