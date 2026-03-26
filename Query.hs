module Query where

import BaseDeDatos

-- Definición de tipos para consultas
data Query =
    From String
  | Select [String] Query
  | Filter Predicate Query
  | Agg AggOp Query
  deriving (Show)

data Predicate =
    Equals String Value
  deriving (Show)

data AggOp =
    Count
  | MaxCol String
  deriving (Show)

-- Función para ejecutar una consulta
runQuery :: DataBase -> Query -> Either DBError [Row]
runQuery db (From name) =
  case revisarTabla name db of
    Left  err   -> Left err
    Right table -> Right (rows table)
runQuery db (Select cols q) =
  case runQuery db q of
    Left  err  -> Left err
    Right rows -> Right (map (selectCols cols) rows)
runQuery db (Filter p q) =
  case runQuery db q of
    Left  err  -> Left err
    Right rows -> Right (filter (evalPred p) rows)
runQuery db (Agg op q) =
  case runQuery db q of
    Left  err  -> Left err
    Right rows -> Right [aggregate op rows]

-- Auxiliares para runQuery
selectCols :: [String] -> Row -> Row
selectCols cols (Row pairs) =
  Row [ (k,v) | (k,v) <- pairs, k `elem` cols ]

evalPred :: Predicate -> Row -> Bool
evalPred (Equals col val) row =
  revisarValor col row == Just val

aggregate :: AggOp -> [Row] -> Row
aggregate Count rs =
  Row [("count", Entero (length rs))]
aggregate (MaxCol col) rs =
  let values = [ v | r <- rs, Just v <- [revisarValor col r] ]
  in case values of
    [] -> Row [("max", Null)]
    vs -> Row [("max", maximum vs)]
