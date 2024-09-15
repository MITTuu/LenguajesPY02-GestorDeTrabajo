module Main where

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de gestión de reservas de salas"
    menuPrincipal

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "1. Gestión de reserva"
    putStrLn "2. Consultar reserva"
    putStrLn "3. Cancelar o modificar reserva"
    putStrLn "4. Consulta de disponibilidad de sala"
    putStrLn "5. Salir"
    option <- getLine
    case option of
        "1" -> putStrLn "gestionReserva"
        "2" -> putStrLn "consultarReserva"
        "3" -> putStrLn "cancelarOmodificarReserva"
        "4" -> putStrLn "consultaDisponibilidad"
        "5" -> putStrLn "Saliendo..." >> return ()
        _   -> putStrLn "Opción no válida" >> menuPrincipal
