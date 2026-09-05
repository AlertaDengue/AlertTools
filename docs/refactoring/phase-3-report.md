# Relatório da Fase 3 — Camada de acesso a dados

Data: 4 de setembro de 2026

## Resultado

Todo acesso SQL do pacote foi concentrado em `R/data_repository.R`. As funções
públicas recebem conexões DBI explicitamente, valores de entrada são enviados
por binding posicional e as escritas usam staging em lote dentro de transações.
Não existe mais fallback para um objeto global `con`.

## Fronteira de dados

O repositório interno contém operações para:

- notificações e registros individuais;
- municípios, regiões e população municipal;
- clima por estação e clima municipal;
- tweets;
- parâmetros e associação de estações;
- associação regional usada pela distribuição do tempo de geração;
- históricos de alerta para dengue, chikungunya e zika.

O mapeamento das tabelas físicas PostgreSQL e SQLite está centralizado em
`.db_table_config`. Datas armazenadas como números no SQLite legado são
normalizadas para `Date` na fronteira. Os consumidores recebem schemas de
domínio iguais nos dois backends.

## SQL seguro e portável

- Valores de cidades, CID-10, estados, regiões, estações, anos e datas usam
  parâmetros vinculados em `DBI::dbGetQuery()`.
- Identificadores fixos são gerados com `DBI::dbQuoteIdentifier()`.
- A única geração de SQL fora do repositório é a exportação explícita para
  arquivo em `write_alerta()`; seus valores passam por
  `DBI::dbQuoteLiteral(DBI::ANSI(), ...)`.
- Diferenças de placeholders, nomes físicos e representação de datas estão
  encapsuladas na camada interna, sem branches por classe nas funções públicas.
- Entradas inválidas são rejeitadas antes da consulta, inclusive intervalos de
  datas, anos, geocódigos, CIDs e conexões ausentes.

## Escrita transacional

`.repo_write_rows()` grava todas as linhas em uma tabela temporária e executa
um único `INSERT ... SELECT` dentro de `DBI::dbWithTransaction()`. A política de
conflito é obrigatoriamente uma destas opções:

- `error`: aborta a operação completa;
- `update`: atualiza as colunas não pertencentes à chave;
- `ignore`: mantém as linhas já existentes.

`write_parameters()` e `write_alerta()` expõem essa política. A compatibilidade
de `overwrite = TRUE` em `write_parameters()` seleciona `update`.
`setWUstation()` usa staging e um único `UPDATE ... FROM`, também transacional.

Os testes verificam escrita em lote, atualização idempotente, conflito,
rollback sem linha parcial e exportação segura de texto contendo tentativa de
injeção SQL.

## Remoção de estado global

Os argumentos `datasource` das funções com acesso ao banco não possuem mais
default. O pipeline propaga a mesma conexão para parâmetros, clima, casos e
persistência. Um teste instala deliberadamente um objeto `con` no ambiente
global e confirma que ele não é consultado.

As transformações `tabela_historico()` e `tabela_historico_intra()` deixaram de
reler parâmetros do banco. Resultados de `pipe_infodengue()` carregam os
parâmetros em um atributo; chamadas diretas devem fornecê-los pelo novo
argumento `parameters`.

## Correções encontradas pelos contratos

- `read.cases()` usava `round(SE / 100)` para encontrar o primeiro ano. Nas
  semanas 52 e 53 isso arredondava para o ano seguinte e descartava casos. A
  extração agora usa `floor(SE / 100)`.
- `getCaseslist()` validava a variável inexistente `dataini`; a validação foi
  removida e o intervalo de datas passou a ser validado e aplicado no banco.
- `getClima()` comparava `vars` consigo mesmo e aceitava nomes desconhecidos;
  agora usa uma lista explícita de variáveis permitidas.
- O pipeline não propagava `datasource` às chamadas internas e tentava escrever
  um objeto inexistente chamado `alerta`; ambos os fluxos foram corrigidos.

## Verificações

| Verificação | Resultado |
|---|---|
| `devtools::test()` | 143 passaram, 1 skip PostgreSQL condicionado, 0 falhas |
| `covr::package_coverage()` | 41,67% |
| ausência de `con` em `R/` | confirmada |
| argumentos `datasource` obrigatórios | confirmados em 16 funções |
| `git diff --check` | passou |
| `R CMD build` | passou |
| `R CMD check --no-manual` | **Status: OK** |

O check foi executado com R 4.4.3 em macOS arm64. `RPostgres` substituiu
`RPostgreSQL` como dependência sugerida para garantir suporte atual ao binding
de parâmetros. A integração PostgreSQL continuará sendo executada no job
efêmero `postgres:16` configurado na CI; não havia daemon Docker local para
reproduzir o serviço nesta máquina.

## Compatibilidade e migração

Esta fase contém mudanças deliberadas de chamada:

- toda função que consulta ou grava banco exige `datasource = connection`;
- `tabela_historico()` e `tabela_historico_intra()` exigem parâmetros em memória
  quando o objeto não veio do pipeline;
- `write_alerta()` e `write_parameters()` aceitam `conflict` explícito.

Nenhum símbolo exportado foi removido e os schemas de retorno protegidos pelas
fixtures foram preservados.

## Gate da fase

O gate foi atendido no código e no backend SQLite: não há concatenação direta
de valores do usuário em SQL, dependência de `con`, leitura usada para escrita
ou escrita linha a linha. Todas as operações do repositório possuem cobertura
de integração. A execução do job PostgreSQL no GitHub Actions permanece como
verificação externa do backend efêmero.

## Próxima seção recomendada

Fase 4 — Núcleo epidemiológico puro.
