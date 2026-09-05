# Plano controlado de migração dos consumidores

Data da última busca: 5 de setembro de 2026.

## Objetivo e limite

Este documento transforma a Fase 7 em uma transição verificável. A busca de
código na organização pública `AlertaDengue` encontrou consumidores reais da
API legada, mas não autoriza alterações nesses repositórios, execução contra
produção, escolha de responsáveis ou publicação de releases.

O registro canônico está em `consumer-registry.csv`. A busca cobre o conteúdo
indexado nos branches padrão acessíveis ao usuário que realizou a auditoria;
chamadas dinâmicas, branches não indexados e repositórios sem acesso ainda
precisam ser confirmados pelos mantenedores.

## Situação encontrada

Os consumidores de maior risco são:

| Consumidor | Uso legado observado | Risco atual |
|---|---|---|
| pipeline nacional (`AlertaDengueAnalise/main/main_BR.R`) | `pipe_infodengue()` e `tabela_historico()` | execução operacional, conexão global e arquivos de saída |
| publicador nacional | `tabela_historico()` e `write_alerta()` | persistência e geração de SQL |
| container de análise | pipeline e persistência legados | imagem de execução operacional |
| cliente R da migração | `pipe_infodengue()` | papel operacional a confirmar |
| pacote e relatórios intramunicipais | `fouralert()` | classificação epidemiológica |
| boletins municipais | `getCases()` e `read.parameters()` | dados e parâmetros operacionais |

A inspeção estática revelou incompatibilidades concretas que precisam ser
tratadas nos consumidores:

- conexões mantidas em `con` global precisam ser passadas como `conn`;
- `pipe_infodengue()` deve ser substituída pelas etapas explícitas
  `fetch_alert_inputs()`, `fetch_alert_parameters()` e
  `run_alert_pipeline()`;
- o resultado moderno é `alerttools_result`, e a persistência requer
  `as_alert_history()` ou `write_alert_results()` com `version_date` explícita;
- geração de arquivo SQL não faz parte da API moderna e precisa de decisão
  própria no consumidor;
- fallbacks por município, paralelismo, artefatos `.RData` e publicação remota
  pertencem ao orquestrador consumidor, não ao núcleo do pacote.

Essas diferenças não devem ser ocultadas reintroduzindo `con` global ou escrita
implícita no AlertTools.

## Ordem de migração

Cada item deve ser migrado em pull request e release próprios, nesta ordem:

1. pipeline nacional em `AlertaDengueAnalise`;
2. publicador nacional e container derivado;
3. cliente R de migração, após confirmar se ainda está ativo;
4. pacote `infodengue_intramunicipal`;
5. relatórios do repositório `intramunicipal`;
6. boletins operacionais;
7. tutoriais, relatórios históricos e pesquisa.

Não avance um consumidor dependente antes de validar o contrato do anterior.

## Procedimento por consumidor

### 1. Baseline

- atribuir responsável e criticidade no registro;
- fixar o commit do consumidor e a versão do AlertTools;
- executar um período epidemiológico representativo em infraestrutura
  descartável;
- guardar apenas schemas, cardinalidades, hashes e métricas não sensíveis;
- registrar depreciações com `options(lifecycle_verbosity = "warning")`.

### 2. Migração

- tornar a conexão DBI explícita e com ciclo de vida local;
- separar leitura, cálculo, transformação e escrita;
- preservar lógica de retentativa, paralelismo e publicação no consumidor;
- trocar cada função conforme `api-migration.md`;
- não alterar simultaneamente regras científicas ou formatação ampla.

O esqueleto esperado para o pipeline municipal é:

```r
inputs <- fetch_alert_inputs(
  conn = conn,
  geocodes = cities,
  disease = disease,
  start_week = start_week,
  report_week = report_week
)
parameters <- fetch_alert_parameters(conn, cities, disease)
result <- run_alert_pipeline(
  inputs,
  parameters,
  report_week = report_week,
  nowcast = nowcast,
  workers = workers
)
history <- as_alert_history(
  result,
  version_date = version_date,
  start_week = history_start_week
)
```

### 3. Validação

- executar testes unitários e de integração do consumidor;
- comparar schemas, tipos, chaves, ordenação e cardinalidades;
- comparar Rt, intervalos, casos estimados e níveis de alerta com tolerâncias
  aprovadas pelo domínio;
- executar ao menos duas viradas de ano e um cenário com semana 53;
- confirmar ausência de warnings de depreciação;
- obter aceite do responsável operacional e, quando aplicável, científico.

### 4. Rollback

- manter a versão anterior do consumidor implantável;
- fixar a última versão aprovada do AlertTools no lockfile ou imagem;
- não alterar tabelas destrutivamente durante a transição;
- definir um critério observável de rollback antes do deploy;
- registrar versão restaurada, motivo e artefatos afetados.

O template de pull request em
`.github/PULL_REQUEST_TEMPLATE/consumer-migration.md` padroniza as evidências.

## Monitoramento e gate

Valide a integridade do registro durante a transição:

```sh
Rscript tools/check-migration-readiness.R
```

Antes de qualquer PR que remova wrappers, execute:

```sh
Rscript tools/check-migration-readiness.R --for-removal
```

O segundo comando falha enquanto a política de suporte não estiver aprovada ou
qualquer consumidor crítico/de alta prioridade não estiver `validated` ou
`retired`. Esse bloqueio é intencional.

Depois de cada release candidata, mantenha `lifecycle_verbosity = "warning"`
nos testes e logs controlados dos consumidores durante a janela de observação.
Registre cada incompatibilidade real no campo `blocker`, com issue, evidência e
decisão; nunca silencie o warning sem migrar ou justificar formalmente.

## Janela de suporte aprovada

A sequência técnica permanece:

- `1.1.1`: saneamento;
- `1.2.0`: camada de dados e API experimental;
- `1.3.0`: API nova estável e início formal dos warnings;
- `1.4.x`: migração e correções de transição;
- `2.0.0`: primeira versão elegível para remoções aprovadas.

A janela aprovada mantém os wrappers por pelo menos duas releases minor e seis
meses, usando o critério que terminar por último. O prazo começa com os warnings
formais da 1.3.0; a 2.0.0 continua sendo apenas a primeira versão elegível para
remoções e ainda depende do gate de consumidores.

## Arquivo da documentação histórica

Ao estabilizar uma release, preserve uma versão do site por tag e mantenha o
guia correspondente à linha suportada. Ao remover wrappers, mova exemplos
legados para uma seção versionada ou release anterior; não apague `NEWS.md`, o
guia de migração, o inventário nem as decisões que expliquem contratos antigos.
