module Main where

import BaseDeDatos
import Query
import Render
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "------------------------Proyecto-2------------------------\n"
  putStrLn "Selecciona una opcion:\n"
  putStrLn "        1-Listar tablas  2-Ver esquema     3-Ver filas"
  putStrLn "        4-Filtrar        5-Conteo          6-Max columna"
  putStrLn "        7-Crear tabla    8-Insertar fila   0-Salir"
  putStrLn"----------------------------------------------------------"
  repl vaciaDB

repl :: DataBase -> IO ()
repl db = do
  putStr "\nfuncdb> "
  hFlush stdout
  cmd <- getLine
  case cmd of
    "0" -> putStrLn "Hasta luego."
    "1" -> do printTables db
              repl db
    "2" -> do t <- pedir "Nombre de tabla: "
              printSchema db t
              repl db
    "3" -> do t <- pedir "Tabla: "
              executeQuery db (From t)
              repl db
    "4" -> do t   <- pedir "Tabla: "
              col <- pedir "Columna: "
              val <- pedir "Valor: "
              executeQuery db (Filter (Equals col (parseValor val)) (From t))
              repl db
    "5" -> do t <- pedir "Tabla: "
              executeQuery db (Agg Count (From t))
              repl db
    "6" -> do t   <- pedir "Tabla: "
              col <- pedir "Columna: "
              executeQuery db (Agg (MaxCol col) (From t))
              repl db
    "7" -> do db' <- crearTablaIO db
              repl db'
    "8" -> do db' <- insertarFilaIO db
              repl db'
    _   -> do putStrLn "Comando no reconocido."
              repl db

-- Ejecuta una consulta pura e imprime
executeQuery :: DataBase -> Query -> IO ()
executeQuery db q =
  case runQuery db q of
    Left  err  -> putStrLn (renderError err)
    Right rs   -> putStr   (renderRows rs)

-- Lista nombres de tablas
printTables :: DataBase -> IO ()
printTables (DataBase []) = putStrLn "(sin tablas)"
printTables (DataBase ts) = mapM_ (putStrLn . tableName) ts

-- Muestra esquema de una tabla
printSchema :: DataBase -> String -> IO ()
printSchema db name =
  case revisarTabla name db of
    Left  err   -> putStrLn (renderError err)
    Right table -> putStrLn (renderSchema table)

-- Crear tabla
crearTablaIO :: DataBase -> IO DataBase
crearTablaIO db = do
  nombre  <- pedir "Nombre de la tabla: "
  colsRaw <- pedir "Columnas (separadas por coma): "
  let columnas = map trim (splitOn ',' colsRaw)
  let tabla    = Tabla nombre columnas []
  putStrLn $ "Tabla '" ++ nombre ++ "' creada."
  return (insertarTable tabla db)

-- Insertar fila
insertarFilaIO :: DataBase -> IO DataBase
insertarFilaIO db = do
  nombre <- pedir "Nombre de la tabla: "
  case revisarTabla nombre db of
    Left  err   -> do putStrLn (renderError err); return db
    Right tabla -> do
      pares <- mapM pedirValor (estructura tabla)
      let tablaActualizada = tabla { rows = rows tabla ++ [Row pares] }
      putStrLn "Fila insertada."
      return (actualizarTabla tablaActualizada db)
  where
    pedirValor col = do
      v <- pedir (col ++ ": ")
      return (col, parseValor v)

-- Revisar string como Value: entero, texto, o NULL si vacio
parseValor :: String -> Value
parseValor "" = Null
parseValor s  = case reads s :: [(Int, String)] of
  [(n, "")] -> Entero n
  _         -> Texto s

-- Pedir input con prompt
pedir :: String -> IO String
pedir msg = do { putStr msg; hFlush stdout; getLine }

-- Dividir string por delimitador con recursion estructural
splitOn :: Char -> String -> [String]
splitOn _ ""  = [""]
splitOn d (c:cs)
  | c == d    = "" : splitOn d cs
  | otherwise = let (h:t) = splitOn d cs in (c:h) : t

-- Eliminar espacios al inicio y final
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')