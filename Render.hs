module Render where

import BaseDeDatos

-- Renderiza una lista de filas como texto plano — función pura
renderRows :: [Row] -> String
renderRows []   = "(sin resultados)"
renderRows rows = unlines (map renderRow rows)

-- Renderiza una sola fila — función pura
renderRow :: Row -> String
renderRow (Row pairs) =
  foldr (\(k,v) acc -> k ++ ": " ++ show v ++ "  " ++ acc) "" pairs

-- Renderiza un error
renderError :: DBError -> String
renderError err = "! " ++ show err

-- Renderiza el esquema de una tabla
renderSchema :: Tabla -> String
renderSchema t =
  "Tabla: " ++ tableName t ++ "\n" ++
  "Columnas: " ++ foldr (\c acc -> c ++ " " ++ acc) "" (estructura t) ++ "\n" ++
  "Filas: " ++ show (rowContador t)