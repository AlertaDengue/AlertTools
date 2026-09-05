# Relatório da Fase 2 — Infraestrutura de testes

Data: 20 de agosto de 2026

## Resultado

A suíte foi reorganizada por responsabilidade e pode ser executada localmente
sem banco de produção, objeto global de conexão, mudança manual do diretório ou
preparação prévia da sessão. O backend rápido usa SQLite em memória; a semântica
PostgreSQL fica coberta por um serviço efêmero dedicado na CI.

## Organização da suíte

- `test-epiweek.R`: calendário epidemiológico, limites de ano, tipos e schemas.
- `test-alert-rules.R`: validação das regras e regressão dos níveis de alerta.
- `test-fetch-cases.R`: validação, contrato e integração de `getCases()`.
- `test-rt.R`: baseline numérico de Rt, com tolerância explícita de `1e-7`.
- `test-fixture-contracts.R`: schemas, chaves e cardinalidade das fixtures.
- `test-utilities.R`: geocódigos, preenchimento, extrapolação e erros de entrada.
- `test-optional-models.R`: disponibilidade condicional de MEM e INLA.
- `helper-fixtures.R` e `helper-database.R`: somente preparação reutilizável.

Os testes legados monolíticos e os skips incondicionais de banco foram
substituídos. Todas as expectativas estão dentro de blocos `test_that()`; os
arquivos de teste não usam `setwd()`, caminhos absolutos, `library()` ou
`require()`.

## Contrato de dados

Foram adicionadas fixtures sintéticas de notificações e da saída semanal
esperada. O teste de `getCases()` verifica:

- nomes e ordem das nove colunas públicas;
- classes tabulares e tipos das colunas;
- cardinalidade semanal e ausência de duplicação da chave
  `(SE, cidade, CID10)`;
- ordenação por semana;
- ausência de valores ausentes nas colunas obrigatórias;
- contagens notificadas, prováveis e confirmadas por laboratório;
- arquivo lateral legado `caselist.RData`, confinado a um diretório temporário;
- erros de validação antes do acesso ao banco.

## Isolamento de banco

O caminho local cria tabelas `Notificacao` e `Municipio` em uma conexão SQLite
em memória e garante desconexão automática, inclusive após falhas.

O job `postgres-integration` da CI inicia `postgres:16`, cria o banco dedicado
`alerttools_test` e executa a suíte completa. O helper recusa qualquer outro
nome de banco, recria somente os schemas `Municipio` e `Dengue_global` dentro
desse banco e registra cleanup automático. O teste PostgreSQL é omitido fora
desse ambiente por `ALERTTOOLS_TEST_POSTGRES=true`.

## Cobertura

A CI ganhou um job separado que calcula `covr::package_coverage()` e publica o
objeto `coverage.rds` como artefato. A linha de base local desta fase é **18,46%**.
O valor foi registrado como indicador para orientar testes futuros, sem impor
uma meta artificial.

## Verificações locais

| Verificação | Resultado |
|---|---|
| `devtools::test()` | 98 passaram, 1 skip PostgreSQL condicionado, 0 falhas |
| `covr::package_coverage()` | 18,46% |
| `R CMD build` | passou |
| `R CMD check --no-manual` | **Status: OK** |

O check foi executado com R 4.4.3 em macOS arm64. O contêiner PostgreSQL não
pôde ser executado localmente porque o daemon Docker não estava disponível; o
workflow dedicado é a validação reproduzível desse caminho e ainda precisa ser
observado em uma execução do GitHub Actions.

## Gate da fase

O gate local foi atendido: qualquer colaborador com as dependências sugeridas
pode executar a suíte determinística sem infraestrutura externa. PostgreSQL,
MEM e INLA possuem condições explícitas e não exigem acesso a sistemas de
produção.

## Próxima seção recomendada

Fase 3 — Camada de acesso a dados.
