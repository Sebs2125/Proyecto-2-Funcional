module Main where

import BaseDeDatos
import Query
import Render
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "=== FuncDB - base de datos funcional en Haskell ==="
  putStrLn "Comandos: 1-Listar tablas  2-Ver esquema  3-Ver todo"
  putStrLn "          4-Filtrar IT     5-Conteo       6-Max salario  0-Salir"
  repl sampleDB

repl :: DataBase -> IO ()
repl db = do
  putStr "\nfuncdb> "
  hFlush stdout
  cmd <- getLine
  case cmd of
    "0" -> putStrLn "Hasta luego."
    "1" -> do printTables db
              repl db
    "2" -> do putStr "Nombre de tabla: "
              hFlush stdout
              t <- getLine
              printSchema db t
              repl db
    "3" -> do executeQuery db (From "empleados")
              repl db
    "4" -> do executeQuery db queryFiltradoIT
              repl db
    "5" -> do executeQuery db queryConteo
              repl db
    "6" -> do executeQuery db queryMaxSalario
              repl db
    _   -> do putStrLn "Comando no reconocido."
              repl db

-- Ejecuta una consulta pura e imprime
executeQuery :: DataBase -> Query -> IO ()
executeQuery db q =
  case runQuery db q of
    Left  err  -> putStrLn (renderError err)
    Right rows -> putStr   (renderRows rows)

-- Lista nombres de tablas
printTables :: DataBase -> IO ()
printTables (DataBase ts) =
  mapM_ (putStrLn . tableName) ts

-- Muestra esquema de una tabla
printSchema :: DataBase -> String -> IO ()
printSchema db name =
  case revisarTabla name db of
    Left  err   -> putStrLn (renderError err)
    Right table -> putStrLn (renderSchema table)

-- Solo empleados de IT, con nombre y salario
queryFiltradoIT :: Query
queryFiltradoIT =
  Select ["nombre", "salario"]
  . Filter (Equals "depto" (Texto "IT"))
  $ From "empleados"

-- Conteo total de empleados
queryConteo :: Query
queryConteo =
  Agg Count
  $ From "empleados"

-- Salario máximo de empleados de RH
queryMaxSalario :: Query
queryMaxSalario =
  Agg (MaxCol "salario")
  . Filter (Equals "depto" (Texto "RH"))
  $ From "empleados"