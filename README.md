# AlertTools

[![R-CMD-check](https://github.com/AlertaDengue/AlertTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlertaDengue/AlertTools/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/346099537.svg)](https://zenodo.org/doi/10.5281/zenodo.13326581)

AlertTools reúne as rotinas usadas pelo InfoDengue para organizar dados
epidemiológicos e climáticos, corrigir atrasos de notificação, estimar Rt e
classificar níveis de alerta. A API moderna separa leitura, cálculo e escrita;
o núcleo também pode ser usado sem banco de dados.

## Requisitos

- R 3.5 ou mais recente;
- dependências regulares instaladas automaticamente pelo gerenciador de pacotes;
- RSQLite apenas para o exemplo local com banco;
- RPostgres apenas para conexão com PostgreSQL;
- INLA e mem somente para os modelos opcionais correspondentes.

Nenhum exemplo desta página exige credenciais, dados de produção ou um objeto
global chamado `con`.

## Instalação

Instale a versão de desenvolvimento diretamente do GitHub:

```r
install.packages("pak")
pak::pak("AlertaDengue/AlertTools")
```

Para desenvolver o pacote a partir de um clone:

```r
pak::pak()
devtools::load_all()
```

## Exemplo mínimo sem banco

O pacote inclui um exemplo autocontido que cria dados sintéticos, executa o
pipeline e deixa o resultado no objeto `result`:

```r
example_file <- system.file(
  "examples", "in-memory-pipeline.R",
  package = "AlertTools",
  mustWork = TRUE
)
sys.source(example_file, envir = environment())

result
summary(result)
head(as.data.frame(result))
```

O fluxo equivalente é composto por duas chamadas principais:

```r
inputs <- new_alert_inputs(cases, climate)

result <- run_alert_pipeline(
  inputs = inputs,
  parameters = parameters,
  report_week = 202512,
  nowcast = "none",
  workers = 1
)
```

## Fluxo com banco

Leitura, processamento e escrita são operações separadas:

```r
inputs <- fetch_alert_inputs(
  conn = conn,
  geocodes = 3304557,
  disease = "dengue",
  start_week = 202501,
  report_week = 202512
)

parameters <- fetch_alert_parameters(
  conn = conn,
  geocodes = 3304557,
  disease = "dengue"
)

result <- run_alert_pipeline(inputs, parameters, nowcast = "none")

write_alert_results(
  conn = conn,
  result = result,
  version_date = as.Date("2026-09-05"),
  conflict = "update"
)
```

O arquivo `inst/examples/sqlite-pipeline.R` demonstra esse fluxo com um banco
SQLite temporário e dados sintéticos.

## Documentação

- `vignette("in-memory-pipeline", package = "AlertTools")`: uso reproduzível
  sem banco;
- `vignette("database-pipeline", package = "AlertTools")`: fronteira DBI com
  SQLite temporário e adaptação para PostgreSQL;
- [guia de migração](docs/refactoring/api-migration.md): correspondência entre
  a API legada e a API em `snake_case`;
- [transição dos consumidores](docs/refactoring/consumer-migration.md): ordem,
  validações e rollback da migração operacional;
- [guia de contribuição](CONTRIBUTING.md): testes, estilo e revisão;
- [processo de release](docs/RELEASE.md): versionamento e publicação.

## Desenvolvimento rápido

```r
devtools::document()
devtools::test()
devtools::check()
```

Veja [CONTRIBUTING.md](CONTRIBUTING.md) antes de alterar regras
epidemiológicas, schemas públicos ou interfaces depreciadas.

## Licença

Consulte o arquivo `LICENSE`. Qualquer mudança de licença exige decisão
explícita dos mantenedores.
