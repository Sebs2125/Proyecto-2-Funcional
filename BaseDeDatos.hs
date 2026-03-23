module BaseDeDatos where

--Tipo de suma: debe ser un valor entero, texto o nulo
data Value = Entero Int | Texto String | Null deriving (Eq, Ord)

--Tipo producto: una fila es una lsita de pares que puede ser (columna, valor)
data Row = Row [(String, Value)]
    deriving (Eq)

--Tipo producto: una tabla debe tener un nombre, estructura y filas
data Tabla = Tabla
    { tableName :: String
    , estructura :: [String]
    , rows :: [Row]
    }

--Tipo suma: la base de datos es una lista de tablas
data DataBase = DataBase [Table]

--Tipo suma: errores propios, sin excepciones
data DBError =
    TablaNotFound String
   | ColumnaNotFound String
   | TypeMismatch String
   | ResultadoVacio
   deriving (Eq)

--Instancias:

instance Show Value where
    show ( Entero n ) = show n
    show ( Texto s ) = s
    show Null = "NULL"

instance Show Row where
    show ( Row pares ) = unwords ( map (\ ( k,v ) -> k ++ "=" ++ show v ) pares

instance Show DBError where
    show ( TablaNotFound t ) = "Error: tabla '" ++ t ++ "' no existe"
    show ( ColumnaNotFound c ) = "Error: columna '" ++ c ++ "' no encontrada"
    show ( TypeMismatch m ) = "Error de tipo: " ++ m
    show ResultadoVacio = "Error: resultado vacio"

--Operaciones referida a almacen de la DB

--Base de datos vacia
vaciaDB :: DataBase
vaciaDB = DataBase []

--Buscar tabla por nombre: recursividad de manera estructural en una lista
revisarTabla :: String -> DataBase -> Either DBError Tabla
revisarTabla nombre ( Database [] ) = Left ( TablaNotFound nombre )
revisarTabla nombre ( Database ( t:ts ) )
  | tablaNombre t == nombre = Right t
  | otherwise = revisarTabla nombre ( Database ts )

-- Buscar valor de una columna en una fila con recursión por pares
revisarValor :: String -> Row -> Maybe Value
revisarValor _   ( Row [] ) = Nothing
revisarValor col ( Row ( ( k,v ):resto ) )
  | k == col  = Just v
  | otherwise = revisarValor col ( Row resto )

-- Insertar tabla en la base de datos
insertarTable :: Table -> Database -> Database
insertarTable t ( Database ts ) = Database ( t : ts )

-- Contar filas usando fold
rowContador :: Table -> Int
rowContador t = foldr (\_ acc -> acc + 1) 0 ( rows t )

--Ejemplo de Base de Datos:
sampleDB :: Database
sampleDB = insertTable deptos (insertTable empleados emptyDB)
  where
    empleados = Table "empleados" ["id", "nombre", "depto", "salario"]
      [ Row [("id", VInt 1), ("nombre", VText "Ana"),    ("depto", VText "IT"), ("salario", VInt 80000)]
      , Row [("id", VInt 2), ("nombre", VText "Carlos"), ("depto", VText "HR"), ("salario", VInt 65000)]
      , Row [("id", VInt 3), ("nombre", VText "Beatriz"),("depto", VText "IT"), ("salario", VInt 90000)]
      , Row [("id", VInt 4), ("nombre", VText "David"),  ("depto", VText "HR"), ("salario", VInt 70000)]
      ]
    deptos = Table "deptos" ["nombre", "piso"]
      [ Row [("nombre", VText "IT"), ("piso", VInt 3)]
      , Row [("nombre", VText "HR"), ("piso", VInt 1)]
      ]