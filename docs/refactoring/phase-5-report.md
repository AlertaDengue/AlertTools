# Relatório da Fase 5 — Nova API e compatibilidade

Data: 4 de setembro de 2026

## Resultado

O pacote agora oferece um fluxo público coerente em `snake_case`, separado em
construção ou leitura de entradas, cálculo, transformação e persistência. A API
nova reproduz o resultado protegido pelo baseline, enquanto as funções legadas
continuam disponíveis como adaptadores com avisos graduais de depreciação.

## Fluxo público mínimo

Foram implementadas as funções exigidas pelo plano:

- `new_alert_inputs()` constrói e valida entradas em memória;
- `fetch_alert_inputs()` busca casos, registros individuais e clima;
- `fetch_alert_parameters()` busca parâmetros por município e doença;
- `run_alert_pipeline()` executa apenas cálculo;
- `write_alert_results()` persiste explicitamente em transação.

O fluxo é complementado por `as_alert_history()` e
`upsert_alert_parameters()`. A leitura granular pode ser feita com
`fetch_cases()`, `fetch_case_records()` e `fetch_climate()`.

Argumentos públicos usam consistentemente `conn`, `geocodes`, `start_date`,
`end_date`, `start_week`, `report_week`, `disease`, `workers` e `verbose`.
Nomes de domínio para dengue, chikungunya e zika são traduzidos para CID-10
canônico na fronteira.

## Contrato de entradas

`new_alert_inputs()` retorna a classe experimental `alerttools_inputs`, com:

- `cases`: chave única `cidade`, `CID10`, `SE`;
- `climate`: chave única `geocodigo`, `SE`;
- `population`: população opcional normalizada;
- `case_records`: registros individuais opcionais;
- `metadata`: proveniência e período.

O construtor valida schemas, tipos numéricos, semanas epidemiológicas,
população positiva, unicidade das chaves e listas nomeadas. Casos e clima são
ordenados de forma determinística. Populações históricas usam o ano mais
recente por município.

## API computacional

Também foram expostos nomes consistentes para responsabilidades puras:

- `estimate_rt()`;
- `nowcast_cases()`;
- `define_alert_rules()`;
- `classify_alerts()`.

Essas funções e a API antiga chamam as mesmas implementações internas. O
pipeline também usa esses pontos únicos, eliminando duplicação de regras.

## Compatibilidade gradual

As seguintes interfaces são wrappers depreciados desde 1.1.0:

- `getCases()` e `getCaseslist()`;
- `getClima()`;
- `read.parameters()` e `write_parameters()`;
- `Rt()` e `adjustIncidence()`;
- `setCriteria()` e `fouralert()`;
- `pipe_infodengue()`;
- `tabela_historico()`;
- `write_alerta()`.

Os wrappers preservam assinaturas, defaults e formatos de retorno e emitem
`lifecycle::deprecate_warn()` indicando a substituição. O pipeline legado agora
compõe a nova construção de entradas, execução e escrita explícita em vez de
manter uma segunda implementação.

A tabela completa de funções, argumentos, colunas e diferenças intencionais
está em `docs/refactoring/api-migration.md`.

## Diferenças intencionais

- A API nova exige `conn` em operações externas e nunca consulta estado global.
- Períodos são explícitos e consistentes entre datas e semanas.
- `run_alert_pipeline()` não possui opção de escrita.
- `write_alert_results()` recebe um resultado ou histórico e aplica uma
  política de conflito explícita.
- Exportação de SQL para arquivo permanece somente na API legada.
- Entradas inválidas são rejeitadas em vez de receber fallback oculto.
- As fachadas de inputs e orquestração estão marcadas como experimentais.

## Testes de migração

`tests/testthat/test-api.R` cobre:

- construção, schema, ordenação e erros de `alerttools_inputs`;
- equivalência exata entre `run_alert_pipeline()` e o pipeline puro aprovado;
- leitura integrada de inputs e parâmetros em SQLite;
- persistência de resultados e parâmetros;
- tradução de argumentos legados;
- avisos com a função substituta;
- equivalência dos wrappers de casos, registros, clima, parâmetros, Rt,
  nowcasting, regras, classificação e histórico.

## Verificações

| Verificação | Resultado |
|---|---|
| `devtools::test()` | 250 passaram, 1 skip PostgreSQL condicionado, 0 falhas ou warnings |
| testes específicos da nova API | 49 passaram |
| `covr::package_coverage()` | 57,75% |
| `git diff --check` | passou |
| `R CMD build` | passou |
| `R CMD check --no-manual` | **Status: OK**, sem erros, avisos ou notas |

O check foi executado com R 4.4.3 em macOS arm64. A integração PostgreSQL
continua condicionada ao serviço efêmero configurado na CI.

## Gate da fase

O gate foi atendido: a API nova reproduz os resultados aprovados, a API antiga
permanece funcional por wrappers e as diferenças intencionais e instruções de
migração estão documentadas.

## Próxima seção recomendada

Fase 6 — Documentação e experiência de desenvolvimento.
