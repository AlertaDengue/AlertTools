# Baseline da Fase 0

Data do levantamento: 20 de agosto de 2026

Commit de referência: `975f347` (`master`)

Ambiente observado: R 4.4.3, macOS arm64

## Escopo desta entrega

Esta primeira implementação cobre o que pode ser reproduzido apenas com o repositório local:

- inventário dos 36 símbolos exportados e dos consumidores internos;
- fixtures sintéticas de casos, clima, população e parâmetros;
- resultados de referência para calendário epidemiológico, Rt e classificação de alertas;
- testes de caracterização que não acessam banco nem gravam arquivos.

Não foram alteradas funções do pacote. Correções de namespace, testes preexistentes, SQL, estado global e efeitos colaterais pertencem às fases seguintes.

## Baseline da suíte preexistente

Comando:

```sh
Rscript -e 'devtools::test()'
```

Resultado antes das adições desta fase:

| Métrica | Resultado |
|---|---:|
| Falhas | 2 |
| Warnings | 0 |
| Skips | 0 |
| Expectativas aprovadas | 49 |

Falhas preexistentes:

1. `test_alertfunctions.R` chama `setCriteria()` fora de `test_that()` e a função não encontra `assert_that` no namespace.
2. `test_timeseriespipeline.R` chama `getCases()` fora de `test_that()` e procura o objeto global `con`.

Após as adições da Fase 0, a suíte completa preserva as mesmas duas falhas e passa em 77 expectativas, das quais 28 pertencem ao novo baseline.

Um novo build e check foram executados em diretório temporário, sem reutilizar `AlertTools.Rcheck` ou o tarball presentes na raiz:

```sh
R CMD build /caminho/para/alertools
R CMD check --no-manual AlertTools_1.1.0.tar.gz
```

O resultado reproduz o baseline do plano: 2 errors, 9 warnings e 6 notes. Entre as causas estão documentação inválida, licença `CCO`, dependências ausentes, chamadas `library()`/`require()` no pacote, símbolos globais e falhas dos testes acima. O check também não conseguiu consultar os índices CRAN/Bioconductor devido à rede restrita do ambiente; isso não alterou as contagens finais, mas deve ser repetido pela CI com rede disponível.

## Cenário sintético de referência

As fixtures descrevem um município fictício, com geocódigo estruturalmente válido `3304557`, população constante de 10.000 habitantes e dengue (`A90`). A série vai de `202052` a `202106`, cobrindo:

- mudança do ano epidemiológico;
- semana epidemiológica 53;
- semanas sem casos e com incidência acima dos dois limiares;
- mudança das condições climáticas;
- ativação e desligamento dos níveis de alerta.

Os dados são integralmente sintéticos e não contêm registros individuais ou informações identificáveis. O município, população e valores não devem ser usados como dado operacional.

Arquivos:

- `tests/testthat/fixtures/cases.csv`;
- `tests/testthat/fixtures/climate.csv`;
- `tests/testthat/fixtures/population.csv`;
- `tests/testthat/fixtures/parameters.csv`;
- `tests/testthat/fixtures/expected-epiweeks.csv`;
- `tests/testthat/fixtures/expected-rt.csv`;
- `tests/testthat/fixtures/expected-alerts.csv`.

## Classificação dos comportamentos

| Comportamento | Classificação inicial | Evidência | Validação pendente |
|---|---|---|---|
| Semana `202053` começa em 2020-12-27 e `202101` em 2021-01-03 | esperado | tabela interna `SE`, `SE2date()` e `episem()` concordam | responsável de domínio |
| `sevendigitgeocode(330455)` retorna `3304557` | esperado | implementação e testes preexistentes | não |
| `Rt()` mantém as colunas de entrada e acrescenta `Rt`, `lwr`, `upr`, `p1` | esperado | execução sobre fixture | responsável de domínio para tolerâncias |
| Primeiros cinco valores de Rt são `NA` com `meangt = 3` | legado | loop começa em `ceiling(2 * meangt)` | confirmar se é contrato ou detalhe interno |
| `fouralert()` retorna classe S3 `alerta` com `data`, `indices`, `crit`, `n` | esperado | execução sobre fixture | não |
| Nível vermelho aparece em `202105`, após condições vermelhas em `202103` e `202104` | legado potencialmente surpreendente | ordem de cálculo e `delay_turnoff` em `fouralert()` | responsável de domínio deve aprovar ou classificar como bug |
| `getCases()` grava `caselist.RData` | legado a remover | chamada `save()` em `R/get_timeseries.R` | já identificado no plano |
| Funções de banco procuram `con` global | bug arquitetural conhecido | defaults `datasource = con` | já identificado no plano |

## Tolerâncias numéricas

Os resultados de Rt são comparados com tolerância absoluta de `1e-7`. Essa tolerância protege contra pequenas diferenças de ponto flutuante sem aceitar mudança substantiva na fórmula. Ela deve ser revista com os responsáveis científicos antes de estabilizar a nova API.

## Gate e pendências humanas

O baseline local é reproduzível sem banco de produção. O gate completo da Fase 0 ainda depende de:

- inventariar repositórios e scripts externos da organização AlertaDengue;
- confirmar quais consumidores são críticos e seus responsáveis;
- aprovar os municípios/períodos que formarão fixtures operacionais anonimizadas;
- validar a convenção epidemiológica, resultados científicos e tolerâncias;
- classificar o deslocamento do alerta vermelho descrito acima como comportamento esperado ou bug.

Enquanto essas decisões estiverem pendentes, as fixtures sintéticas funcionam como proteção técnica inicial, não como validação científica final.
