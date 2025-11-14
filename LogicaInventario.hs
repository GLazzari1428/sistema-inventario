module LogicaInventario where
import DadosInventario
-- Importa função para manipular o map
import qualified Data.Map as Map
import Data.Time (UTCTime)

type ResultadoOperacao = (Inventario, LogEntry)

-- Adicionar novo item ao inventario
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem timestamp novoItem inventario = 
    let idDoItem = itemID novoItem
    
    in if Map.member idDoItem inventario
        then
            Left "Erro: ID do item ja existe."
        else
            let novoInventario = Map.insert idDoItem novoItem inventario
            
                logEntry = LogEntry
                    { timestamp = timestamp
                    , acao      = Add
                    , detalhes  = "Item '" ++ (nome novoItem) ++ "' adicionado."
                    , status    = Sucesso
                    }
            in Right (novoInventario, logEntry)
            
-- Remover quantidade de item no inventário
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem timestamp itemID qtdRemover inventario =
    let resultadoBusca = Map.lookup itemID inventario 
    in case resultadoBusca of
        Nothing  ->
            Left "Erro: Item nao encontrado."
        Just itemEncontrado ->
            if(quantidade itemEncontrado) < qtdRemover
            then
                Left "Erro: Estoque insuficiente."
            else
                let novaQtd = (quantidade itemEncontrado) - qtdRemover
                    itemAtualizado = itemEncontrado { quantidade = novaQtd }
                    novoInventario = Map.insert itemID itemAtualizado inventario
                    logEntry = LogEntry
                        { timestamp = timestamp
                        , acao      = Remove
                        , detalhes  = "Removidas " ++ (show qtdRemover) ++ " unidades do item '" ++ (nome itemEncontrado) ++ "'."
                        , status    = Sucesso
                        }
                in Right (novoInventario, logEntry)

-- Adicionar nova quantidade de itens ao inventario  
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty timestamp itemID qtdAdicionar inventario =
    let resultadoBusca = Map.lookup itemID inventario
    in case resultadoBusca of
        Nothing   -> 
            Left "Erro: Item nao encontrado."
        Just itemEncontrado ->
            let novaQtd = (quantidade itemEncontrado) + qtdAdicionar
                itemAtualizado = itemEncontrado { quantidade = novaQtd }
                novoInventario = Map.insert itemID itemAtualizado inventario
                logEntry = LogEntry
                    { timestamp = timestamp
                    , acao      = Update
                    , detalhes  = "Adicionadas " ++ (show qtdAdicionar) ++ " unidades do item '" ++ (nome itemEncontrado) ++ "'."
                    , status    = Sucesso
                    }
            in Right (novoInventario, logEntry)