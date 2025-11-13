# Sistema de Gerenciamento de Inventário

## Informações Institucionais
- **Disciplina:** Programação Lógica e Funcional
- **Instituição:** Pontifícia Universidade Católica do Paraná    
- **Professor:** Frank Coelho de Alcantara
- **Turma:** U
- **Equipe:** 10

### Membros da equipe

| Nome Completo | Usuário GitHub |
| :--- | :--- |
| `Gustavo Tasca Lazzari` | `@GLazzari1428` |
| `Mateus Roese Tucunduva`| `@Matizuuu`     |
| `Matheus Yamamoto Dias` | `@MatheusYamas` |
| `Victor Ryuki Tamezava` | `@VicRuk`       |

---

## 1. Objetivo do Projeto

Este projeto consiste no desenvolvimento de um Sistema de Gerenciamento de Inventário em Haskell, utilizando princípios de programação funcional pura e separação clara entre lógica de negócio e operações de entrada/saída. O sistema permite adicionar, remover e atualizar itens de um inventário, mantendo persistência de dados em disco e registrando todas as operações em um log de auditoria. Adicionalmente, o sistema fornece funcionalidades de análise de logs para gerar relatórios sobre o histórico de operações.

---

## 2. Funcionalidades

O sistema foi implementado com as seguintes funcionalidades:

- **Gerenciamento de Itens:** Adicionar, remover e atualizar a quantidade de itens no inventário.
- **Persistência de Estado:** O inventário é salvo automaticamente no arquivo `Inventario.dat` após cada operação bem-sucedida.
- **Log de Auditoria:** Todas as operações (sucesso ou falha) são registradas no arquivo `Auditoria.log` com timestamp, tipo de ação, detalhes e status.
- **Tratamento de Erros:** O sistema valida operações e retorna mensagens de erro claras (ex: estoque insuficiente, item não encontrado, ID duplicado).
- **Relatórios e Análise:** Funções para análise de logs incluindo relatório geral, logs de erro, histórico por item e identificação do item mais movimentado.
- **Separação Lógica Pura/Impura:** As funções de lógica de negócio são puras e utilizam `Either` para sinalizar falhas, enquanto operações de IO são isoladas no módulo principal.

---

## 3. Estrutura do Repositório

O projeto está organizado da seguinte forma para facilitar a navegação e o entendimento:

```bash
.
├── DadosInventario.hs    # Definição de tipos de dados
├── LogicaInventario.hs   # Funções puras de lógica de negócio (addItem, removeItem, updateQty)
├── Relatorios.hs         # Funções de análise de logs e geração de relatórios
├── main.hs               # Módulo principal com IO, persistência e loop de execução
├── Inventario.dat        # Arquivo de dados do inventário que é gerado automaticamente
├── Auditoria.log         # Arquivo de log de auditoria que é gerado automaticamente
└── README.md             # Descrição completa do projeto
```

---

## 4. Como Executar

Para executar o sistema, deve-se utilizar o GDB Online, basta acessar o link abaixo para executar o nosso projeto:

[LINK PARA O REPOSITÓRIO GDB ONLINE DO PROJETO](https://onlinegdb.com/eFlXb_uz4S)


---

## 5. Comandos Disponíveis

O sistema aceita os seguintes comandos no terminal:

| Comando | Descrição | Exemplo |
| :--- | :--- | :--- |
| `add [id] [nome] [qtd] [categoria]` | Adiciona um novo item ao inventário | `add T001 Teclado 10 Perifericos` |
| `remove [id] [qtd]` | Remove uma quantidade específica de um item | `remove T001 3` |
| `update [id] [qtd]` | Adiciona uma quantidade a um item existente | `update T001 5` |
| `listar` | Lista todos os itens do inventário | `listar` |
| `report` | Exibe relatório geral e logs de erro | `report` |
| `report [id]` | Exibe o histórico de operações de um item específico | `report T001` |
| `ajuda` | Exibe a lista de comandos disponíveis | `ajuda` |
| `sair` | Encerra o programa | `sair` |

---

## 6. Exemplo de Uso

Abaixo um exemplo prático de interação com o sistema, demonstrando todos os comandos e funcionalidades:

```
Carregando sistema de inventário...
Aviso: Inventario.dat não encontrado. Iniciando com inventário vazio.
Aviso: Auditoria.log não encontrado. Iniciando com log vazio.
Sistema carregado. Digite 'ajuda' para comandos.
> add T001 Teclado 10 Perifericos
Sucesso: Item 'Teclado' adicionado.
> add M001 Mouse 15 Perifericos
Sucesso: Item 'Mouse' adicionado.
> add M002 Monitor 5 Monitores
Sucesso: Item 'Monitor' adicionado.
> add HD001 HD_Externo 20 Armazenamento
Sucesso: Item 'HD_Externo' adicionado.
> add T002 Teclado_USB 8 Perifericos
Sucesso: Item 'Teclado_USB' adicionado.
> add T001 Teclado_Mecanico 5 Perifericos
Erro: ID do item ja existe.
> listar
--- Inventário Atual ---
Item {itemID = "HD001", nome = "HD_Externo", quantidade = 20, categoria = "Armazenamento"}
Item {itemID = "M001", nome = "Mouse", quantidade = 15, categoria = "Perifericos"}
Item {itemID = "M002", nome = "Monitor", quantidade = 5, categoria = "Monitores"}
Item {itemID = "T001", nome = "Teclado", quantidade = 10, categoria = "Perifericos"}
Item {itemID = "T002", nome = "Teclado_USB", quantidade = 8, categoria = "Perifericos"}
> remove T001 3
Sucesso: Removidas 3 unidades do item 'Teclado'.
> remove M001 5
Sucesso: Removidas 5 unidades do item 'Mouse'.
> remove M002 10
Erro: Estoque insuficiente.
> remove INVALID 2
Erro: Item nao encontrado.
> update T001 5
Sucesso: Adicionadas 5 unidades do item 'Teclado'.
> update HD001 10
Sucesso: Adicionadas 10 unidades do item 'HD_Externo'.
> update INVALID 5
Erro: Item nao encontrado.
> listar
--- Inventário Atual ---
Item {itemID = "HD001", nome = "HD_Externo", quantidade = 30, categoria = "Armazenamento"}
Item {itemID = "M001", nome = "Mouse", quantidade = 10, categoria = "Perifericos"}
Item {itemID = "M002", nome = "Monitor", quantidade = 5, categoria = "Monitores"}
Item {itemID = "T001", nome = "Teclado", quantidade = 12, categoria = "Perifericos"}
Item {itemID = "T002", nome = "Teclado_USB", quantidade = 8, categoria = "Perifericos"}
> report

=== RELATORIO GERAL ===
Total de operacoes: 14
Operacoes bem-sucedidas: 10
Operacoes com falha: 4

Distribuicao por tipo:
  - Adicoes (Add): 6
  - Remocoes (Remove): 4
  - Atualizacoes (Update): 3
=======================

=== LOGS DE ERRO ===
- 2025-11-13 20:38:41.061298132 UTC | Add | Adicao [T001] Teclado_Mecanico | Erro: Erro: ID do item ja existe.
- 2025-11-13 20:39:01.642030744 UTC | Remove | Remocao [M002] qtd=10 | Erro: Erro: Estoque insuficiente.
- 2025-11-13 20:39:05.12665899 UTC | Remove | Remocao [INVALID] qtd=2 | Erro: Erro: Item nao encontrado.
- 2025-11-13 20:39:14.542131843 UTC | Update | Atualizacao [INVALID] qtd=5 | Erro: Erro: Item nao encontrado.
====================

Item mais movimentado: T001 (3 operacoes)

> report T001

=== HISTORICO DO ITEM: T001 ===
- 2025-11-13 20:38:23.397480714 UTC | Add | [T001] Item 'Teclado' adicionado. | Sucesso
- 2025-11-13 20:38:41.061298132 UTC | Add | Adicao [T001] Teclado_Mecanico | Falha "Erro: ID do item ja existe."
- 2025-11-13 20:38:54.622174902 UTC | Remove | [T001] Removidas 3 unidades do item 'Teclado'. | Sucesso
- 2025-11-13 20:39:08.028181813 UTC | Update | [T001] Adicionadas 5 unidades do item 'Teclado'. | Sucesso
=============================

> sair
Saindo.
```

---

## 7. Cenários de Teste

Os seguintes cenários de teste foram executados para validar o sistema:

### 7.1. Cenário 1: Persistência de Estado (Sucesso)

**Passos Executados:**
1. Iniciar o programa (sem arquivos de dados existentes)
2. Adicionar 3 itens:
   - `add T001 Teclado 10 Perifericos`
   - `add M001 Mouse 15 Perifericos`
   - `add M002 Monitor 5 Monitores`
3. Fechar o programa com o comando `sair`
4. Verificar a existência dos arquivos *Inventario.dat* e *Auditoria.log*
5. Reiniciar o programa
6. Executar o comando `listar` para verificar se os 3 itens foram carregados

**Resultado:** 
```haskell
Carregando sistema de inventário...
Aviso: Inventario.dat não encontrado. Iniciando com inventário vazio.
Aviso: Auditoria.log não encontrado. Iniciando com log vazio.
Sistema carregado. Digite 'ajuda' para comandos.
> add T001 Teclado 10 Perifericos
Sucesso: Item 'Teclado' adicionado.
> add M001 Mouse 15 Perifericos
Sucesso: Item 'Mouse' adicionado.
> add M002 Monitor 5 Monitores
Sucesso: Item 'Monitor' adicionado.
> sair
Saindo.

-- APÓS REINICIAR 

Carregando sistema de inventário...
Sistema carregado. Digite 'ajuda' para comandos.
> listar
--- Inventário Atual ---
Item {itemID = "M001", nome = "Mouse", quantidade = 15, categoria = "Perifericos"}
Item {itemID = "M002", nome = "Monitor", quantidade = 5, categoria = "Monitores"}
Item {itemID = "T001", nome = "Teclado", quantidade = 10, categoria = "Perifericos"}
```

### 7.2. Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Passos Executados:**
1. Adicionar um item com 10 unidades:
   - `add T003 Teclado_Mecanico 10 Perifericos`
2. Tentar remover 15 unidades desse item:
   - `remove T003 15`
3. Verificar se o programa exibiu mensagem de erro clara
4. Verificar se o *Inventario.dat* (e o estado em memória) ainda mostra 10 unidades.
5. Verificar que o arquivo *Auditoria.log* contém uma `LogEntry` com `StatusLog (Falha ...)`

**Resultado:**
```haskell
> add T003 Teclado_Mecanico 10 Perifericos
Sucesso: Item 'Teclado_Mecanico' adicionado.
> remove T003 15
Erro: Estoque insuficiente.
> listar 
--- Inventário Atual ---
Item {itemID = "T003", nome = "Teclado_Mecanico", quantidade = 10, categoria = "Perifericos"}
> sair
Saindo.
```

Arquivo *Auditoria.log*:
```log
LogEntry {timestamp = 2025-11-13 22:40:59.408536943 UTC, acao = Initialize, detalhes = "Sistema inicializado", status = Sucesso}
LogEntry {timestamp = 2025-11-13 22:41:02.047753538 UTC, acao = Add, detalhes = "[T003] Item 'Teclado_Mecanico' adicionado.", status = Sucesso}
LogEntry {timestamp = 2025-11-13 22:41:11.472585093 UTC, acao = Remove, detalhes = "Remocao [T003] qtd=15", status = Falha "Erro: Estoque insuficiente."}
```

Arquivo *Inventario.dat*:

```dat
fromList [("T003",Item {itemID = "T003", nome = "Teclado_Mecanico", quantidade = 10, categoria = "Perifericos"})]
```

---

### 7.3. Cenário 3: Geração de Relatório de Erros

**Passos Executados:**
1. Executar o Cenário 2 (falha de estoque insuficiente registrada no log)
2. Executar o comando `report` para visualizar o relatório geral
4. Verificar se a seção "LOGS DE ERRO" exibe todas as entradas de falha

**Resultado:**
```haskell
Carregando sistema de inventário...
Sistema carregado. Digite 'ajuda' para comandos.
> report

=== RELATORIO GERAL ===
Total de operacoes: 4
Operacoes bem-sucedidas: 3
Operacoes com falha: 1

Distribuicao por tipo:
  - Adicoes (Add): 1
  - Remocoes (Remove): 1
  - Atualizacoes (Update): 0
=======================


=== LOGS DE ERRO ===
- 2025-11-13 22:46:51.318894367 UTC | Remove | Remocao [T003] qtd=15 | Erro: Erro: Estoque insuficiente.
====================


Item mais movimentado: T003 (1 operacoes)
```

Arquivo *Auditoria.log*:
```log
LogEntry {timestamp = 2025-11-13 22:46:39.95348616 UTC, acao = Initialize, detalhes = "Sistema inicializado", status = Sucesso}
LogEntry {timestamp = 2025-11-13 22:46:45.505646028 UTC, acao = Add, detalhes = "[T003] Item 'Teclado_Mecanico' adicionado.", status = Sucesso}
LogEntry {timestamp = 2025-11-13 22:46:51.318894367 UTC, acao = Remove, detalhes = "Remocao [T003] qtd=15", status = Falha "Erro: Estoque insuficiente."}
LogEntry {timestamp = 2025-11-13 22:47:03.708996956 UTC, acao = Initialize, detalhes = "Sistema inicializado", status = Sucesso}
LogEntry {timestamp = 2025-11-13 22:47:11.54430646 UTC, acao = Report, detalhes = "Comando report executado", status = Sucesso}
```

