# Inventário inicial da API do AlertTools

Baseline levantado no commit `975f347` em 20 de agosto de 2026. Este inventário descreve a API exportada pelo `NAMESPACE` atual; não constitui aprovação dos contratos nem autorização para remover interfaces.

## Resumo

- 36 funções exportadas.
- Há funções que combinam acesso direto ou indireto a banco com cálculo e orquestração.
- 16 funções exportadas usam `datasource = con` como default explícito.
- O pipeline principal não propaga `datasource` para todas as chamadas internas.
- Há funções exportadas de baixo nível e nomes com quatro convenções diferentes: `camelCase`, `snake_case`, nomes pontuados e siglas.

## Funções exportadas

| Função | Arquivo | Argumentos atuais | Retorno/efeito observado ou documentado | Grupo | Estado no baseline |
|---|---|---|---|---|---|
| `Rt()` | `R/Rt_functions.R` | `obj, count, gtdist, meangt, sdgt, CI, alpha, a0, b0` | data frame de entrada acrescido de `Rt`, `lwr`, `upr` e `p1` | núcleo | caracterizado com fixture |
| `SE2date()` | `R/utility_tools.R` | `se` | data frame `SE`, `ini` | calendário | caracterizado com fixture |
| `adjustIncidence()` | `R/adjust_incidence_functions.R` | `obj, datas, method, pdig, Dmax, nyears, datasource, nowSE, safelimit, ...` | série acrescida de estimativas de casos | núcleo misturado com dados | teste atual depende de INLA/dados |
| `bayesnowcasting()` | `R/adjust_incidence_functions.R` | `d, Dmax, nowSE, nweeks, interacao, tweet` | resultado de nowcasting | núcleo | dependência opcional INLA |
| `bestWU()` | `R/get_timeseries.R` | `series, var` | seleção/composição de séries meteorológicas | núcleo auxiliar | não caracterizado |
| `data2SE()` | `R/utility_tools.R` | `days, format` | vetor numérico de semanas | calendário | caracterizado com fixture |
| `daySEday()` | `R/utility_tools.R` | `x, format` | data frame `SE`, `ini` | calendário | teste existente |
| `epiYear()` | `R/utility_tools.R` | `se, cut` | tibble com `se`, `year`, `eweek`, `eyear` | calendário | teste existente |
| `episem()` | `R/utility_tools.R` | `x, format, separa, retorna` | semana/ano numérico ou texto | calendário | caracterizado com fixture |
| `fouralert()` | `R/alert_functions.R` | `obj, crit, miss, dy` | objeto S3 `alerta` | regras de alerta | caracterizado com fixture |
| `getCases()` | `R/get_timeseries.R` | `cities, lastday, firstday, cid10, dataini, completetail, type, datasource` | série semanal; grava `caselist.RData` | leitura com efeito colateral | depende de banco e `con` |
| `getCaseslist()` | `R/get_timeseries.R` | `cities, lastday, firstday, cid10, datasource` | registros individuais | leitura | depende de banco e `con` |
| `getCidades()` | `R/utility_tools.R` | `regional, macroregional, uf, datasource` | tabela de municípios e regiões | leitura | depende de banco e `con` |
| `getClima()` | `R/get_timeseries.R` | `cities, vars, finalday, iniSE, lastSE, datasource` | série climática municipal | leitura/cálculo | depende de banco e `con` |
| `getPop()` | `R/get_timeseries.R` | `cities, iniY, endY` | tabela `geocode`, `year`, `pop` | leitura externa | depende de `brpop` |
| `getRegionais()` | `R/utility_tools.R` | `cities, uf, sortedby, macroreg, datasource, output` | vetor ou tabela de regiões | leitura | depende de banco e `con` |
| `getTweet()` | `R/get_timeseries.R` | `cities, lastday, cid10, datasource` | série semanal de tweets | leitura | depende de banco e `con` |
| `getWU()` | `R/get_timeseries.R` | `stations, vars, finalday, iniSE, datasource` | série climática por estação | leitura/cálculo | depende de banco e `con` |
| `getWUstation()` | `R/utility_tools.R` | `cities, datasource` | associação município-estação | leitura | depende de banco e `con` |
| `getdelaydata()` | `R/adjust_incidence_functions.R` | `cities, nyears, cid10, lastday, datasource` | dados de atraso de notificação | leitura | depende de banco e `con` |
| `infodengue_apply_mem()` | `R/infodengue_apply_mem.R` | `mun_list, start_year, end_year, write, database, passwd, ...` | parâmetros MEM por município; escrita opcional | cálculo/dados/persistência | depende de banco e `mem` |
| `infodengue_apply_mem_agreg()` | `R/infodengue_apply_mem.R` | `mun_list, start_year, end_year, nome, database, passwd, ...` | parâmetros MEM agregados | cálculo/dados | depende de banco e `mem` |
| `mem_curve()` | `R/infodengue_apply_mem.R` | `mun_list, start_year, end_year, nome, database, passwd, ...` | curva MEM agregada | cálculo/dados | depende de banco e `mem` |
| `nafill()` | `R/utility_tools.R` | `v, rule, maxgap, verbose` | vetor com ausências tratadas | núcleo auxiliar | teste existente |
| `pipe_infodengue()` | `R/alert_functions.R` | `cities, cid10, datarelatorio, finalday, iniSE, nowcasting, narule, writedb, datasource, completetail, dataini` | lista nomeada de objetos `alerta`; escrita opcional | orquestração | depende de banco, arquivo e `con` |
| `read.cases()` | `R/get_timeseries.R` | `start_year, end_year, cid10, datasource, mun_list` | casos semanais para MEM | leitura | depende de banco e `con` |
| `read.parameters()` | `R/utility_tools.R` | `cities, cid10, datasource` | tabela de parâmetros | leitura | depende de banco e `con` |
| `seqSE()` | `R/utility_tools.R` | `from, to` | fatia da tabela interna `SE` | calendário | caracterizado com fixture |
| `setCriteria()` | `R/alert_functions.R` | `rule, values, delays` | lista `crity`, `crito`, `critr` | regras de alerta | caracterizado com fixture |
| `setWUstation()` | `R/utility_tools.R` | `st, UF, datasource` | atualização de associação de estações | persistência | depende de banco e `con` |
| `sevendigitgeocode()` | `R/utility_tools.R` | `dig` | geocódigo municipal de sete dígitos | validação | caracterizado com fixture |
| `tabela_historico()` | `R/alert_functions.R` | `obj, iniSE, lastSE, type, versao` | tabela para histórico municipal | transformação/dados | relê parâmetros do banco |
| `tabela_historico_intra()` | `R/alert_functions.R` | `obj, iniSE, lastSE, versao` | tabela para histórico intramunicipal | transformação/dados | relê parâmetros do banco |
| `temp.predict()` | `R/utility_tools.R` | `v, plotar` | vetor com cauda prevista | núcleo auxiliar | teste existente; referência inválida a `forecast::fitted` |
| `write_alerta()` | `R/alert_functions.R` | `d, writetofile, datasource, arq` | escreve SQL/arquivo ou banco | persistência | depende de banco/arquivo e `con` |
| `write_parameters()` | `R/utility_tools.R` | `city, cid10, params, overwrite, datasource` | escreve e retorna parâmetros | persistência | depende de banco e `con` |

## Uso interno conhecido

| Consumidor local | Funções/contratos consumidos | Criticidade | Responsável | Observação |
|---|---|---|---|---|
| `R/alert_functions.R` | `data2SE`, `SE2date`, `read.parameters`, `getClima`, `getCases`, `adjustIncidence`, `Rt`, `setCriteria`, `fouralert`, `write_alerta` | alta | a confirmar | compõe o pipeline operacional |
| `R/Rt_functions.R` | `getWU`, `nafill`, `GenTimeDist` e consultas diretas | média | a confirmar | `GenTimeDist` não está exportada, mas participa do núcleo |
| `R/infodengue_apply_mem.R` | `read.cases`, tabelas de população, DBI e `mem` | alta | a confirmar | combina cálculo, leitura e escrita |
| `tests/testthat/test_alertfunctions.R` | `setCriteria`, `getCases`, `getWU`, `Rt`, `fouralert` | média | testes | executa código fora de `test_that()` e requer `con` |
| `tests/testthat/test_timeseriespipeline.R` | `getCases`, `getWU`, `adjustIncidence`, `Rt` | média | testes | requer `con` e INLA |
| `tests/testthat/test_utilityfunctions.R` | utilitários de calendário, geocódigo, preenchimento e funções de banco | média | testes | 49 expectativas passam no baseline atual |
| Exemplos em `man/` e roxygen | maioria da API exportada | média | documentação | vários exemplos pressupõem `con` |

## Consumidores externos

O repositório local não contém referências a `AlertTools::` nem permite concluir quais sistemas externos usam o pacote. Antes da Fase 5, os mantenedores devem completar a tabela abaixo com pesquisa na organização AlertaDengue.

| Consumidor | Repositório/script | Funções | Colunas lidas | Criticidade | Responsável | Situação |
|---|---|---|---|---|---|---|
| A confirmar | A confirmar | A confirmar | A confirmar | A confirmar | A confirmar | inventário externo pendente |

## Atualização da Fase 7

Uma busca de código na organização pública `AlertaDengue`, realizada em 5 de
setembro de 2026, confirmou consumidores externos da API legada. O inventário
operacional, com arquivos, criticidade, evidência, bloqueios e rollback, passou
a ser mantido em `docs/refactoring/consumer-registry.csv`; o procedimento está
em `docs/refactoring/consumer-migration.md`.

Entre os consumidores críticos encontrados estão o pipeline nacional em
`AlertaDengueAnalise`, seu publicador e container, o cliente R de migração,
rotinas intramunicipais e boletins. A linha histórica acima permanece intacta
para registrar que esses consumidores ainda eram desconhecidos no baseline.

## Contratos que não podem mudar sem validação

- Convenção brasileira de semanas epidemiológicas, principalmente viradas de ano e semana 53.
- Fórmulas, intervalos e tolerâncias numéricas de Rt.
- Regras, delays e precedência dos quatro níveis de alerta.
- Nomes, tipos, chaves e ordenação das tabelas consumidas externamente.
- Interpretação dos CID-10 equivalentes para dengue, chikungunya e zika.
- Política de casos notificados, prováveis e confirmados por laboratório.
