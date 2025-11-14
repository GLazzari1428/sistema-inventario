module Main where

import DadosInventario
import LogicaInventario
import Relatorios

import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

-- SALVAR: inventário
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)

-- SALVAR: append log
salvarLogEntry :: LogEntry -> IO ()
salvarLogEntry entry = appendFile logFile (show entry ++ "\n")

-- CARREGAR: inventário
carregarInventario :: IO Inventario
carregarInventario =
  (do
    conteudo <- readFile inventarioFile
    length conteudo `seq` return ()
    case readMaybe conteudo of
      Just inv -> return inv
      Nothing -> do
        putStrLn "Aviso: Houve um erro ao ler Inventario.dat. Iniciando inventário vazio."
        return Map.empty
  ) `catch` \e ->
    if isDoesNotExistError e
    then do
      putStrLn "Aviso: Inventario.dat não encontrado. Iniciando com inventário vazio."
      return Map.empty
    else ioError e

-- CARREGAR: logs
carregarLogs :: IO [LogEntry]
carregarLogs =
  (do
    conteudo <- readFile logFile
    length conteudo `seq` return ()
    let linhas = lines conteudo
        parsed = [ x | ln <- linhas, Just x <- [readMaybe ln] ]
        descartadas = length linhas - length parsed
    if descartadas > 0
      then putStrLn $ "Aviso: " ++ show descartadas ++ " linha(s) inválida(s) em Auditoria.log foram ignoradas."
      else return ()
    return parsed
  ) `catch` \e ->
    if isDoesNotExistError e
    then do
      putStrLn "Aviso: Auditoria.log não encontrado. Iniciando com log vazio."
      return []
    else ioError e

-- FALHA IO
logFalhaIO :: UTCTime -> String -> String -> [LogEntry] -> IO [LogEntry]
logFalhaIO ts comando msg logs = do
  let le = LogEntry
        { timestamp = ts
        , acao = QueryFail
        , detalhes = "Comando=[" ++ comando ++ "]"
        , status = Falha msg
        }
  salvarLogEntry le
  return (logs ++ [le])

type Resultado = (Inventario, [LogEntry])

-- FALHA: exibe erro, loga Falha e mantém estado antigo
tratarResultado :: UTCTime
                -> AcaoLog
                -> String  -- itemID for context
                -> String  -- descricao for failures
                -> Inventario
                -> [LogEntry]
                -> Either String ResultadoOperacao
                -> IO Resultado
tratarResultado time acao itemId falhaMsg inv logs (Left erro) = do
  putStrLn erro
  let logFalha = LogEntry time acao falhaMsg (Falha erro)
  salvarLogEntry logFalha
  return (inv, logs ++ [logFalha])

-- SUCESSO: exibe msg, enriquece log with [itemID], salva inventário
tratarResultado _ _ itemId _ _ logs (Right (novoInv, logSucesso)) = do
  let detalhesEnriquecido = "[" ++ itemId ++ "] " ++ detalhes logSucesso
      logEnriquecido = logSucesso { detalhes = detalhesEnriquecido }

  putStrLn $ "Sucesso: " ++ detalhes logSucesso
  salvarInventario novoInv
  salvarLogEntry logEnriquecido  -- Save the ENRICHED log
  return (novoInv, logs ++ [logEnriquecido])

-- MAIN
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Carregando sistema de inventário..."
  estadoInicialInv <- carregarInventario
  estadoInicialLogs <- carregarLogs
  now <- getCurrentTime

  -- LOG DE INICIALIZAÇÃO
  let initLog = LogEntry now Initialize "Sistema inicializado" Sucesso
  salvarLogEntry initLog
  let logs1 = estadoInicialLogs ++ [initLog]

  putStrLn "Sistema carregado. Digite 'ajuda' para comandos."
  loop estadoInicialInv logs1

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "> "
  comando <- getLine
  if comando == "sair"
  then do
    putStrLn "Saindo."
    exitSuccess
  else do
    (novoInv, novoLogs) <- processarComando comando inv logs
    loop novoInv novoLogs

-- PROCESSAR COMANDO
processarComando :: String -> Inventario -> [LogEntry] -> IO Resultado
processarComando comando inv logs = do
  time <- getCurrentTime
  case words comando of

    -- ADD
    ["add", id', nome', qtdStr, cat'] -> do
      case readMaybe qtdStr of
        Nothing -> do
          putStrLn "Erro de input: Quantidade deve ser um numero."
          novoLogs <- logFalhaIO time comando "Quantidade não numerica." logs
          return (inv, novoLogs)
        Just qtd -> do
          let item = Item { itemID = id', nome = nome', quantidade = qtd, categoria = cat' }
          tratarResultado time Add id' ("Adicao [" ++ id' ++ "] " ++ nome') inv logs (addItem time item inv)

    -- REMOVE
    ["remove", id', qtdStr] -> do
      case readMaybe qtdStr of
        Nothing -> do
          putStrLn "Erro de input: Quantidade deve ser um numero."
          novoLogs <- logFalhaIO time comando "Quantidade nao numerica." logs
          return (inv, novoLogs)
        Just qtd ->
          tratarResultado time Remove id' ("Remocao [" ++ id' ++ "] qtd=" ++ show qtd) inv logs (removeItem time id' qtd inv)

    -- UPDATE
    ["update", id', qtdStr] -> do
      case readMaybe qtdStr of
        Nothing -> do
          putStrLn "Erro de input: Quantidade deve ser um numero."
          novoLogs <- logFalhaIO time comando "Quantidade nao numerica." logs
          return (inv, novoLogs)
        Just qtd ->
          tratarResultado time Update id' ("Atualizacao [" ++ id' ++ "] qtd=" ++ show qtd) inv logs (updateQty time id' qtd inv)

    -- LISTAR
    ["listar"] -> do
      putStrLn "--- Inventário Atual ---"
      if Map.null inv
        then putStrLn "Inventário vazio."
        else mapM_ print (Map.elems inv)
      return (inv, logs)

    -- REPORT
    ["report"] -> do
      putStrLn ""
      putStrLn $ relatorioGeral logs
      putStrLn ""
      putStrLn $ exibirLogsDeErro (logsDeErro logs)
      putStrLn ""
      putStrLn $ exibirItemMaisMovimentado logs
      putStrLn ""

      let reportLog = LogEntry time Report "Comando report executado" Sucesso
      salvarLogEntry reportLog
      return (inv, logs ++ [reportLog])

    -- REPORT [itemID]
    ["report", itemId] -> do
      putStrLn ""
      putStrLn $ exibirHistoricoPorItem itemId logs
      putStrLn ""

      let reportLog = LogEntry time Report ("Comando report para item: " ++ itemId) Sucesso
      salvarLogEntry reportLog
      return (inv, logs ++ [reportLog])

    -- AJUDA
    ["ajuda"] -> do
      putStrLn "--- Comandos ---"
      putStrLn "add [id] [nome] [qtd] [categoria]"
      putStrLn "remove [id] [qtd]"
      putStrLn "update [id] [qtd_adicionar]"
      putStrLn "listar"
      putStrLn "report"
      putStrLn "report [id]"
      putStrLn "sair"
      putStrLn "----------------"
      return (inv, logs)

    -- Comando inválido
    _ -> do
      putStrLn "Comando inválido. Digite 'ajuda'."
      novoLogs <- logFalhaIO time comando "Sintaxe inválida." logs
      return (inv, novoLogs)
