module Main where

import Data.Map (Map)
import Data.Time (UTCTime)

-- 1. Item
data Item = Item
    { itemID    :: String
    , nome      :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)

-- 2. Inventario
type Inventario = Map String Item

-- 3. AcaoLog (ADT)
data AcaoLog
    = Add
    | Remove
    | Update
    | Report
    | QueryFail
    | Initialize
    deriving (Show, Read, Eq)

-- 4. StatusLog (ADT)
data StatusLog
    = Sucesso
    | Falha String
    deriving (Show, Read, Eq)

-- 5. LogEntry
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao      :: AcaoLog
    , detalhes  :: String
    , status    :: StatusLog
    } deriving (Show, Read, Eq)