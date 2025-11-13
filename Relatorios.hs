module Relatorios where

import DadosInventario
import qualified Data.Map as Map
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing, Down(..))

-- Filtra logs de erro (status = Falha)
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro entry = case status entry of
      Falha _ -> True
      _       -> False

-- Retorna o historico de operacoes para um item especifico
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId = filter mencionaItem
  where
    mencionaItem entry = 
      ("[" ++ itemId ++ "]") `elem` words (detalhes entry)

-- Identifica o item mais movimentado com base nos logs
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let 
    -- Extrai operacoes relevantes COM SUCESSO apenas
    logsRelevantes = filter isOperacaoSucesso logs

    -- Extrai IDs de itens dos detalhes (formato: [ItemID])
    idsExtraidos = concatMap extrairItemID logsRelevantes

    -- Conta frequencias
    frequencias = contarFrequencias idsExtraidos

  in
    if null frequencias
    then Nothing
    else Just $ maximumBy (comparing snd) frequencias
  where
    isOperacaoSucesso entry = 
      acao entry `elem` [Add, Remove, Update] && status entry == Sucesso

    -- Extrai o ID do item dos detalhes do log
    -- Formato esperado: "Adicao [T001] Teclado" ou "Item 'Nome' adicionado."
    extrairItemID :: LogEntry -> [String]
    extrairItemID entry = 
      let det = detalhes entry
      in extrairEntreBrackets det

    -- Extrai texto entre [colchetes]
    extrairEntreBrackets :: String -> [String]
    extrairEntreBrackets [] = []
    extrairEntreBrackets ('[':resto) = 
      case break (== ']') resto of
        (id', ']':resto') -> id' : extrairEntreBrackets resto'
        (_, []) -> []
        _ -> []
    extrairEntreBrackets (_:resto) = extrairEntreBrackets resto

    contarFrequencias :: [String] -> [(String, Int)]
    contarFrequencias [] = []
    contarFrequencias ids = 
      let grupos = groupBy (==) (sortBy compare ids)
      in [(x, length grp) | grp@(x:_) <- grupos]

--  gera um relatorio de estatisticas gerais
relatorioGeral :: [LogEntry] -> String
relatorioGeral logs =
  let
    totalOps = length logs
    sucessos = length $ filter isSucesso logs
    falhas = length $ filter isErro logs
    adds = length $ filter (\e -> acao e == Add) logs
    removes = length $ filter (\e -> acao e == Remove) logs
    updates = length $ filter (\e -> acao e == Update) logs
  in unlines
    [ "=== RELATORIO GERAL ==="
    , "Total de operacoes: " ++ show totalOps
    , "Operacoes bem-sucedidas: " ++ show sucessos
    , "Operacoes com falha: " ++ show falhas
    , ""
    , "Distribuicao por tipo:"
    , "  - Adicoes (Add): " ++ show adds
    , "  - Remocoes (Remove): " ++ show removes
    , "  - Atualizacoes (Update): " ++ show updates
    , "======================="
    ]
  where
    isSucesso entry = status entry == Sucesso
    isErro entry = case status entry of
      Falha _ -> True
      _       -> False

-- Exibe logs de erro de forma formatada
exibirLogsDeErro :: [LogEntry] -> String
exibirLogsDeErro erros =
  if null erros
  then "Nenhum erro registrado."
  else unlines $ "=== LOGS DE ERRO ===" : map formatarErro erros ++ ["===================="]
  where
    formatarErro entry = 
      let msg = case status entry of
            Falha m -> m
            _       -> "N/A"
      in "- " ++ show (timestamp entry) ++ " | " ++ show (acao entry) ++ 
         " | " ++ detalhes entry ++ " | Erro: " ++ msg

-- Exibe historico de um item especifico
exibirHistoricoPorItem :: String -> [LogEntry] -> String
exibirHistoricoPorItem itemId logs =
  let historico = historicoPorItem itemId logs
  in if null historico
     then "Nenhuma operacao encontrada para o item: " ++ itemId
     else unlines $ ("=== HISTORICO DO ITEM: " ++ itemId ++ " ===") : 
          map formatarLog historico ++ ["============================="]
  where
    formatarLog entry = 
      "- " ++ show (timestamp entry) ++ " | " ++ show (acao entry) ++ 
      " | " ++ detalhes entry ++ " | " ++ show (status entry)

-- exibe o item mais movimentado
exibirItemMaisMovimentado :: [LogEntry] -> String
exibirItemMaisMovimentado logs =
  case itemMaisMovimentado logs of
    Nothing -> "Nenhum item movimentado encontrado."
    Just (itemId, count) -> 
      "Item mais movimentado: " ++ itemId ++ " (" ++ show count ++ " operacoes)"
