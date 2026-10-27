# Migração para a API `snake_case`

Versão de início da depreciação: 1.1.0.

A API nova separa leitura, cálculo e persistência. Funções experimentais estão
identificadas na ajuda do pacote e podem receber ajustes compatíveis antes de
serem declaradas estáveis. A API legada permanece disponível durante a janela
de migração e emite avisos `lifecycle` com a substituição indicada.

Consumidores confirmados, ordem de transição, evidências e rollback são
mantidos em `consumer-migration.md` e `consumer-registry.csv`. A política de
suporte ainda está proposta; nenhuma data de remoção foi aprovada.

## Fluxo mínimo

| Etapa | API nova | Contrato |
|---|---|---|
| Construir dados em memória | `new_alert_inputs()` | retorna `alerttools_inputs` |
| Buscar dados operacionais | `fetch_alert_inputs()` | somente leitura; `conn` explícito |
| Buscar parâmetros | `fetch_alert_parameters()` | uma linha por município e doença |
| Executar cálculo | `run_alert_pipeline()` | sem banco ou arquivos; retorna `alerttools_result` |
| Converter histórico | `as_alert_history()` | conversão explícita com `version_date` |
| Persistir | `write_alert_results()` | transação e conflito explícitos |

## Funções e argumentos

| API legada | API nova | Adaptação principal | Situação |
|---|---|---|---|
| `getCases()` | `fetch_cases()` | `datasource` → `conn`; `cities` → `geocodes`; `cid10` → `disease`; `firstday`/`lastday` → `start_date`/`end_date`; `dataini` → `case_date` | wrapper depreciado desde 1.1.0 |
| `getCaseslist()` | `fetch_case_records()` | mesmos argumentos de conexão, local, doença e datas | wrapper depreciado desde 1.1.0 |
| `getClima()` | `fetch_climate()` | `vars` → `climate_vars`; datas explícitas | wrapper depreciado desde 1.1.0 |
| `read.parameters()` | `fetch_alert_parameters()` | `datasource` → `conn`; `cities` → `geocodes`; nomes de doenças aceitos | wrapper depreciado desde 1.1.0 |
| `pipe_infodengue()` | `fetch_alert_inputs()` + `run_alert_pipeline()` | leitura e cálculo passam a ser chamadas separadas; escrita removida do cálculo | wrapper depreciado desde 1.1.0 |
| `Rt()` | `estimate_rt()` | argumentos longos em `snake_case` | wrapper depreciado desde 1.1.0 |
| `adjustIncidence()` | `nowcast_cases()` | `datas` → `case_records`; `nowSE` → `report_week` | wrapper depreciado desde 1.1.0 |
| `setCriteria()` | `define_alert_rules()` | `delays` preservado | wrapper depreciado desde 1.1.0 |
| `fouralert()` | `classify_alerts()` | `crit` → `rules`; `miss` → `missing`; `dy` → `minimum_history` | wrapper depreciado desde 1.1.0 |
| `tabela_historico()` | `as_alert_history()` | `versao` → `version_date`; `iniSE`/`lastSE` → `start_week`/`end_week` | wrapper depreciado desde 1.1.0 |
| `write_alerta()` | `write_alert_results()` | recebe diretamente `alerttools_result`; não exporta arquivo implicitamente | wrapper depreciado desde 1.1.0 |
| `write_parameters()` | `upsert_alert_parameters()` | aceita lote; `overwrite` é substituído por `conflict` | wrapper depreciado desde 1.1.0 |

As demais funções legadas permanecem estáveis nesta fase porque não são
necessárias ao fluxo mínimo comprovado. Novos nomes somente serão adicionados
quando houver contrato e cenário de uso testado, evitando ampliar a API sem
necessidade observada.

## Valores de doença

O argumento `disease` aceita nomes de domínio ou os códigos equivalentes:

| Nome | CID-10 canônico | Valores aceitos |
|---|---|---|
| dengue | `A90` | `dengue`, `A90` |
| chikungunya | `A92.0` | `chikungunya`, `chik`, `A92`, `A920`, `A92.0` |
| zika | `A92.8` | `zika`, `A928`, `A92.8` |

## Schemas públicos

### `alerttools_inputs`

Campos: `cases`, `climate`, `population`, `case_records` e `metadata`.

- `cases`: chave única `cidade`, `CID10`, `SE`; contagens `casos`, `cas_prov`,
  `cas_lab`; população positiva em `pop`; ordenação por cidade, doença e semana;
- `climate`: chave única `geocodigo`, `SE`; ordenação por município e semana;
- `population`: opcional, normalizada para `cidade`, `pop`;
- `case_records`: registros individuais opcionais para nowcasting;
- `metadata`: lista nomeada com proveniência e período.

### `alerttools_result`

Campos: `data`, `alerts`, `parameters`, `metadata` e `diagnostics`. `data` e
`alerts` possuem a mesma cardinalidade. A combinação tabular é obtida com
`as.data.frame()`.

### Histórico persistível

`as_alert_history()` retorna uma linha por `SE`, `municipio_geocodigo` e
`Localidade_id`, incluindo contagens, incidência, Rt, nível, indicadores
climáticos e `versao_modelo`.

## Diferenças intencionais

- Toda leitura ou escrita nova exige `conn`; não existe fallback global.
- Períodos são explícitos e datas são objetos `Date`.
- `run_alert_pipeline()` nunca persiste resultados.
- `write_alert_results()` não oferece exportação SQL para arquivo; esse recurso
  permanece apenas no wrapper legado enquanto houver consumidores.
- A API nova rejeita doença desconhecida, chaves duplicadas, população ausente
  ou não positiva e períodos contraditórios, em vez de aplicar fallback oculto.
- `workers = 1` e `verbose = FALSE` são os padrões portáveis.

## Remoção e documentação histórica

Nenhum wrapper pode ser removido enquanto
`Rscript tools/check-migration-readiness.R --for-removal` falhar. Além da
aprovação da janela de suporte, consumidores críticos precisam estar validados
ou formalmente aposentados. Documentação de versões antigas será preservada por
tag ou site versionado, sem apagar o histórico deste guia e do `NEWS.md`.
