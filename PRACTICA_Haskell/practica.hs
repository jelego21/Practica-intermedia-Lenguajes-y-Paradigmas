import Text.Printf(printf)
import System.IO--read write files
import System.Directory (doesFileExist)
type Student =  (String, Int, Maybe Int)--(id, in, out)

-- checkIn
checkIn :: [Student] -> IO [Student]
checkIn lista = do
    putStrLn "--Ingresando al campus--"
    putStrLn "ID del estudiante: "
    studentId <- getLine

    let alreadyIn = any (\(id,_,salida) -> id == studentId && salida == Nothing) lista
    if alreadyIn then do
        putStrLn "El ID ya esta registrado"
        return lista
    else do
        putStrLn "Hora de entrada (HH:MM): "
        horaInStr <- getLine
        let horaIn = aMin horaInStr
        let new = (studentId, horaIn, Nothing)
        putStrLn "Entrada registrada"

        let nuevaLista = new : lista
        saveStudents nuevaLista
        return nuevaLista
-- checkOut
checkOut :: [Student] -> IO [Student]
checkOut lista = do
    putStrLn "--Saliendo del campus--"
    putStrLn "ID del estudiante: "
    studentId <- getLine

    let inCampus = filter (\(id,_,salida) -> id == studentId && salida == Nothing) lista
    case inCampus of
        [] -> do
            putStrLn "El ID no está en el campus o no existe"
            return lista
        ((id, entrada, _):_) -> do
            putStrLn "Hora de salida (HH:MM): "
            horaOutStr <- getLine
            let horaOut = aMin horaOutStr
                nuevaLista = map (\(id, entrada, salida) ->
                    if id == studentId
                        then (id, entrada, Just horaOut)
                        else (id, entrada, salida)
                    ) lista
            putStrLn "Se registró la salida"
            saveStudents nuevaLista
            return nuevaLista

-- conversion de tiempo hora -> min
aMin :: String -> Int
aMin input =
    let (hString, mString) = span (/=':') input
        horas = read hString :: Int
        minutos = read (tail mString) :: Int
    in horas * 60 + minutos

-- conversion de tiempo min -> hora
aHour :: Int -> String
aHour mins =
    let h = mins `div` 60
        m = mins `mod` 60
    in printf "%02d:%02d" h m

-- calculo de tiempo in
tiempoIn :: Student -> String
tiempoIn (id, entrada, salida) =
    case salida of
        Nothing -> "El estudiante esta en el campus"
        Just x ->
            let total = x-entrada
            in "Duración: " ++ aHour total ++ " horas"

--filtrado/busqueda
buscar :: [Student] -> IO()
buscar lista = do
    putStrLn "ID del estudiante: "
    busquedaId <- getLine
    let result = filter (\(id,_,_) -> id == busquedaId) lista
    case result of
        [] -> putStrLn "ID no encontrado"
        ((id, entrada, salida):_) -> do
            putStrLn $ "ID: " ++ id
            putStrLn $ "Hora de entrada: " ++ aHour entrada
            case salida of
                Nothing -> putStrLn "El estudiante sigue dentro del campus"
                Just x -> do
                    putStrLn $ "Salida: " ++ aHour x
                    putStrLn $ tiempoIn (id, entrada, salida)
--mostrar todos los estudiantes
mostrar :: [Student] -> IO()
mostrar lista = do
    putStrLn "Estudiantes registrados:"
    mapM_ (\(id, entrada, salida)->do    
        putStrLn $ "Estudiante:  " ++ id
        putStrLn $ "Hora de entrada: " ++ aHour entrada
        case salida of
            Nothing -> putStrLn "El estudiante sigue dentro del campus"
            Just x -> putStrLn $ "Hora de salida: " ++ aHour x
        putStrLn ""
        ) lista
--archivos
filePath :: FilePath
filePath = "University.txt"

studentToString :: Student -> String
studentToString (id, entrada, salida) =
    id ++ "," ++ aHour entrada ++ "," ++ formatSalida salida

formatSalida :: Maybe Int -> String
formatSalida Nothing = "Nothing"
formatSalida (Just x) = aHour x
stringToStudent str =
    let (id, rest1) = span (/= ',') str
        rest2 = tail rest1
        (entradaStr, rest3) = span (/= ',') rest2
        salidaStr = tail rest3
    in (id, aMin entradaStr, parseSalida salidaStr)

parseSalida :: String -> Maybe Int
parseSalida "Nothing" = Nothing
parseSalida x = Just (aMin x)

loadStudents :: IO [Student]
loadStudents = do
    exists <- doesFileExist filePath
    if not exists then return []
    else do
        content <- readFile filePath
        let ls = lines content
        return (map stringToStudent ls)

saveStudents :: [Student] -> IO ()
saveStudents lista = do
    let content = unlines (map studentToString lista)
    writeFile filePath content
--main
main :: IO ()
main = do
    lista <- loadStudents
    loop lista
    where
        loop :: [Student] -> IO ()
        loop lista = do
            putStrLn "--- Menu ---"
            putStrLn "1. Registrar entrada"
            putStrLn "2. Buscar estudiante"
            putStrLn "3. Registrar salida"
            putStrLn "4. Listar estudiantes"
            putStrLn "0. Salir"
            option <- getLine
            case option of
                "1" -> do
                    newList <- checkIn lista
                    loop newList
                "2" -> do
                    buscar lista
                    loop lista
                "3" -> do
                    newList <- checkOut lista
                    loop newList
                "4" -> do
                    mostrar lista
                    loop lista
                "0" -> putStrLn "Saliendo..."
                _   -> do
                    putStrLn "Opción no válida, intente de nuevo."
                    loop lista
