module BaseDeDatos where

-- Tipo de suma: debe ser un valor entero, texto o nulo
data Value = Entero Int | Texto String | Null deriving (Eq, Ord)

-- Tipo producto: una fila es una lsita de pares que puede ser (columna, valor)
data Row = Row [(String, Value)]
  deriving (Eq)

-- Tipo producto: una tabla debe tener un nombre, estructura y filas
data Tabla = Tabla
  { tableName :: String,
    estructura :: [String],
    rows :: [Row]
  }

-- Tipo suma: la base de datos es una lista de tablas
data DataBase = DataBase [Tabla]

-- Tipo suma: errores propios, sin excepciones
data DBError
  = TablaNotFound String
  | ColumnaNotFound String
  | TypeMismatch String
  | ResultadoVacio
  deriving (Eq)

-- Instancias:

instance Show Value where
  show (Entero n) = show n
  show (Texto s) = s
  show Null = "NULL"

instance Show Row where
  show (Row pares) = unwords (map (\(k, v) -> k ++ "=" ++ show v) pares)

instance Show DBError where
  show (TablaNotFound t) = "Error: tabla '" ++ t ++ "' no existe"
  show (ColumnaNotFound c) = "Error: columna '" ++ c ++ "' no encontrada"
  show (TypeMismatch m) = "Error de tipo: " ++ m
  show ResultadoVacio = "Error: resultado vacio"

-- Operaciones referida a almacen de la DB

-- Base de datos vacia
vaciaDB :: DataBase
vaciaDB = DataBase []

-- Buscar tabla por nombre: recursividad de manera estructural en una lista
revisarTabla :: String -> DataBase -> Either DBError Tabla
revisarTabla nombre (DataBase []) = Left (TablaNotFound nombre)
revisarTabla nombre (DataBase (t : ts))
  | tableName t == nombre = Right t
  | otherwise = revisarTabla nombre (DataBase ts)

-- Buscar valor de una columna en una fila con recursión
revisarValor :: String -> Row -> Maybe Value
revisarValor _ (Row []) = Nothing
revisarValor col (Row ((k, v) : resto))
  | k == col = Just v
  | otherwise = revisarValor col (Row resto)

-- Insertar tabla en la base de datos
insertarTable :: Tabla -> DataBase -> DataBase
insertarTable t (DataBase ts) = DataBase (t : ts)

-- Contar filas usando fold
rowContador :: Tabla -> Int
rowContador t = foldr (\_ acc -> acc + 1) 0 (rows t)

-- actualizar una tabla existente
actualizarTabla :: Tabla -> DataBase -> DataBase
actualizarTabla nueva (DataBase ts) =
  DataBase (map reemplazar ts)
  where
    reemplazar t
      | tableName t == tableName nueva = nueva
      | otherwise = t