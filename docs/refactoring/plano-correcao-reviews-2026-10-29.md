# Plano executável de correção dos reviews e reorganização dos PRs

Data: 29/10/2026. Repositório: AlertaDengue/AlertTools.

Estado deste documento: **correções implementadas e validadas localmente; publicação não executada**.

Relatório: [correcoes-review-2026-10-29.md](correcoes-review-2026-10-29.md).
A auditoria Zika em homologação depende de acesso e da lista operacional, não encontrados no repositório.
Este arquivo é a fonte de execução e de passagem de trabalho entre agentes.
Os estados abaixo não certificam trabalho feito. Atualizá-los somente com evidências.

## 1. Resultado esperado e limites

Corrigir os comentários de Sandro nos PRs #28, #30, #31, #32 e #34;
organizar a auditoria do #33; preservar as entregas aprovadas dos #35–37;
reconstruir a cadeia de branches sem perder os merges de #27 e #29.

Ao terminar, cada PR deve conter um diff próprio, testes proporcionais ao seu
escopo, CI aprovado no commit publicado e uma resposta preparada para o review.
Não criar PRs substitutos, mesclar PRs, publicar release, alterar o pin do
consumidor ou escrever em banco operacional como parte deste plano.
Postar respostas ou solicitar revisão apenas quando essa comunicação estiver
autorizada; até lá, entregar os textos prontos no relatório de execução.

Não mudar o algoritmo epidemiológico ou aumentar limites de concorrência para
fazer a validação passar. Não regenerar valores esperados a partir da candidata.
Falhas devem resultar em diagnóstico e correção ou pendência explicitamente registrada.

## 2. Situação verificada e referências iniciais

Estas referências são um retrato, não valores a serem impostos ao remoto.
Revalidar no início da execução. Se o remoto mudou, incorporar a mudança e
registrar os novos limites antes de qualquer rebase ou publicação.

| PR | Branch | Base atual | Revisão inicial / observação |
|---|---|---|---|
| #20 | `codex/alerttools-1.2.0-rc1` | `master` | `57428ed`; contém o merge do #27 |
| #27 | `codex/pr1-characterization-baseline` | candidata | Mesclado; merge `57428ed998032a5d958afeb55a372145cd0b8601` |
| #28 | `codex/pr2-legacy-compat` | candidata | Remoto `7213aa10fc9d8c2c2830406b183f4e4c99c55d10`; já contém o merge do #29 |
| #29 | `codex/pr3-setcriteria-compat` | PR #28 | Mesclado; commit original `e76044d` |
| #30 | `codex/pr4-deprecation-policy` | PR #28 | `0fd9c0d` |
| #31 | `codex/pr5-canonical-inputs` | PR #30 | `c6fe5cc` |
| #32 | `codex/pr6-municipality-disease` | PR #31 | `3d62c68` |
| #33 | `codex/pr7-zika-transition` | PR #32 | `88efaf0`; sem aprovação vigente no estado agregado |
| #34 | `codex/pr8-concurrency-controls` | PR #33 | `e7de3e7` |
| #35 | `codex/pr9-minimum-history-contract` | PR #34 | `2f5ad3e`; aprovado |
| #36 | `codex/pr10-epidemiological-equivalence` | PR #35 | `1534d94`; aprovado |
| #37 | `codex/pr11-consumer-integration` | PR #36 | `15e0915`; aprovado |

`master` verificado: `e460b3b8c15ab28e8755f482d576cf7280803b34`.
A branch local do #28 estava desatualizada e ainda apontava para `9931177`.
**Não usar essa ponta local como origem de uma reconstrução que perca o #29.**

Referências históricas imutáveis para comparação:

- AlertTools usado pelo consumidor: `e460b3b8c15ab28e8755f482d576cf7280803b34`.
- Consumidor AlertaDengueAnalise: `27a03f26a9bf366597ef135e7da8d2f620ea9a8e`.
- Snapshots: `tests/reference/e460b3b8/` e
  `tests/reference/consumer-27a03f26/`; preservar origem e integridade com texto LF/CRLF.

## 3. Organização dos agentes e dependências

Não operar duas branches no mesmo checkout simultaneamente. Se houver execução
concorrente, usar checkouts isolados. Um único integrador controla a cadeia,
rebases e publicação. Agentes especialistas entregam commits identificados;
não fazem rebase ou push de branches pertencentes a outro agente.

| ID | Responsável sugerido | Depende de | Entrega | Estado |
|---|---|---|---|---|
| R00 | Integrador | — | Inventário atualizado, backups e mapa de commits | Validado |
| R01 | Agente de compatibilidade | R00 | Correção e teste mínimo no #28 | Validado |
| R02 | Agente de API/documentação | R00; integração após R01 | Testes de versão e guia corrigido no #30 | Validado |
| R03 | Agente de pipeline | R00; integração após R02 | Granularidade no #31 e proteção legada no #32 | Validado |
| R04 | Agente de INLA | R00 para preparar; R03 para integração | Testes reais, runner e benchmark no #34 | Validado |
| R05 | Agente de homologação | R00; candidata consolidada para executar | Auditoria Zika e relatório do #33 | Pendente de ambiente externo |
| R06 | Integrador | R01–R04 | Cadeia reconstruída, #35–37 preservados | Preparado localmente; aguarda publicação |
| R07 | Agente de validação / integrador | R06 | Evidências finais e CI das novas pontas | Validado localmente; CI remoto após publicação |
| R08 | Integrador | R07; registrar situação de R05 | Matriz de atendimento e respostas prontas | Validado; respostas preparadas |

R02, a preparação de R04 e a coleta de pré-requisitos de R05 podem ocorrer em
paralelo com R01. R01 e R03 alteram `R/alert_functions.R`: integrar em sequência.
R04 e R07 podem precisar do mesmo runner e de recursos de Docker: reservar a execução.
O ambiente externo de R05 não bloqueia correções de código, mas impede declarar
a auditoria concluída e a migração pronta.

## 4. Contrato de entrega obrigatório para cada agente

Ao receber uma tarefa, ler este documento e cumprir somente os IDs atribuídos.
Antes de editar, confirmar branch, commit-base, árvore de trabalho e dependências.
Não sobrescrever trabalho alheio. Preservar `plano_refatoracao_alerttools.html`
e `plano_refatoracao_alerttools.qmd`, que já estavam sem rastreamento.

Entregar ao integrador:

1. ID da tarefa, estado e commit-base completo.
2. Commits produzidos e arquivos alterados.
3. Correspondência entre pedido do review, implementação e teste.
4. Comandos executados, resultados, skips, limitações e evidências.
5. Dependências externas ou decisões ainda necessárias.
6. Texto de resposta ao revisor, sem publicá-lo automaticamente.

Estados permitidos: `A fazer`, `Em execução`, `Implementado; falta validar`,
`Validado`, `Pendente de ambiente externo`, `Preparado localmente; aguarda publicação`
e `Validado localmente; CI remoto após publicação`. Teste ignorado não é teste aprovado.
Não usar contagens antigas de checks como evidência da revisão nova.

## 5. R00 — Atualizar referências e proteger o histórico

### Ações

- Consultar GitHub para estado, head/base e comentários atuais dos PRs #20 e #27–37.
- Atualizar refs remotas sem descartar alterações locais.
- Registrar SHA completo de cada ponta e base em um manifesto de execução.
- Criar refs locais de segurança com prefixo `codex/backup-review-2026-10-29/`.
  Não publicar backups sem necessidade.
- Registrar os limites das séries antigas: #28 inclui #29 remoto; #30 antigo
  contém o commit original do #29; #31–37 formam a cadeia anterior.
- Conferir se há novos commits em `master`. A candidata já contém o `master`
  verificado; não reescrever o #20 sem necessidade.

### Aceite

Manifesto permite recuperar cada branch. Merges #27/#29 identificados.
Mudanças locais preservadas. Nenhuma branch remota reescrita nesta tarefa.

## 6. R01 — Corrigir a perda de parâmetros no #28

Review: https://github.com/AlertaDengue/AlertTools/pull/28#issuecomment-6447839399

### Arquivos de partida

- `R/alert_functions.R`: `.as_legacy_alert_objects()`, `.legacy_alert_parameters()`
  e implementação de histórico.
- `R/api.R`: wrapper `tabela_historico()`.
- Solução existente no commit `15e0915` do #37.
- `tests/testthat/test-api.R`; criar teste focado se necessário.

### Ações

- Extrair do #37 somente a preservação dos parâmetros como atributos dos filhos
  e sua recuperação quando a lista externa perdeu o atributo.
- Manter os campos públicos dos objetos `alerta` e o atributo externo existente.
- Recuperar parâmetros sem nova leitura implícita do banco.
- Testar dois municípios com parâmetros diferentes, concatenados exatamente com
  a operação `do.call(c, ...)` usada no consumidor.
- Comparar histórico combinado com históricos individuais pelas chaves, valores
  e parâmetros; reproduzir o problema antes da correção.
- Cobrir parâmetros ausentes e conflitantes com erro claro. Não escolher
  silenciosamente o primeiro conjunto de parâmetros.
- Manter no #37 o cenário integrado com as funções reais do consumidor.

### Aceite

Concatenação funciona, histórico preservado e nenhuma consulta adicional ao banco.
Defaults legados de conexão e data, formato dos objetos e comportamento do #29 preservados.
O #28 pode ser revisado sem depender de código exclusivo do #37.

## 7. R02 — Corrigir testes e documentação de depreciação no #30

Review: https://github.com/AlertaDengue/AlertTools/pull/30#issuecomment-6448407990

### Arquivos de partida

- `R/api.R`: `.deprecate_api()`.
- `tests/testthat/test-api.R`.
- `docs/refactoring/api-migration.md` e `transition-policy.dcf`.
- `NEWS.md` e documentação gerada, se afetados.

### Ações

- Separar a leitura da versão instalada da decisão de emitir warning, usando
  helper interno testável ou dependência interna substituível.
- Não alterar a assinatura pública dos wrappers nem a versão em `DESCRIPTION`
  para simular testes.
- Testar 1.2.0 e outra versão 1.2.x sem aviso; 1.3.0 e versão posterior com aviso.
- Testar a decisão e ao menos a emissão real via `lifecycle`, controlando sua
  supressão por sessão para que o teste não dependa da ordem de execução.
- Alinhar o guia à política já aprovada e ao início dos avisos em 1.3.0.

### Aceite

Os dois lados da fronteira 1.3.0 são exercitados. Os testes não ocultam todos os
warnings globalmente. Documentação e implementação expressam a mesma política.

## 8. R03 — Tornar #31 e #32 consistentes e independentes

Reviews:

- https://github.com/AlertaDengue/AlertTools/pull/31#issuecomment-6449473906
- https://github.com/AlertaDengue/AlertTools/pull/32#issuecomment-6463503608

### Arquivos de partida

- `R/core_pipeline.R`: normalização, `.validate_pipeline_inputs()`, grupos de
  cálculo e `.filter_case_records_for_group()`.
- `R/alert_functions.R`: `.as_legacy_alert_objects()`.
- `tests/testthat/test-core-pipeline.R`, `test-api.R` e helpers de fixtures.
- Implementação município/doença já existente em `3d62c68`.

### Entrega R03-A, no #31

- Antecipar do #32 o processamento por município + doença e as validações
  necessárias; não deixar a unidade de cálculo diferente da unidade validada.
- Preservar aliases legados apenas nas fronteiras previstas.
- Exigir correspondência dos conjuntos município/doença entre casos e parâmetros,
  incluindo ambos os sentidos; apontar os pares problemáticos no erro.
- Filtrar também os registros individuais de casos por doença. Rejeitar registros
  ambíguos quando um município tiver múltiplas doenças e faltar essa identificação.
- Garantir que o join climático município/semana não multiplique casos.

### Entrega R03-B, no #32

- Retirar do diff o processamento antecipado para o #31.
- Antes de montar qualquer objeto legado, validar que o resultado contém uma
  única doença coerente nos dados e parâmetros; rejeitar múltiplos CID10.
- Manter o processamento de múltiplas doenças disponível na API moderna.
- Atualizar título/descrição do #32 para o escopo residual real.

### Matriz mínima de testes

| Cenário | Resultado esperado |
|---|---|
| Um município, dengue + chikungunya | API moderna calcula grupos separados |
| Dois municípios, pares diferentes de doenças | Mesmas saídas das execuções isoladas, alinhadas por chave |
| Parâmetro ausente ou excedente | Erro anterior ao cálculo, com par identificado |
| Chaves duplicadas em casos, clima ou parâmetros | Erro; nenhuma multiplicação silenciosa |
| Registros individuais sem doença em entrada ambígua | Erro explícito |
| Resultado com múltiplas doenças enviado ao conversor legado | Erro explícito antes da conversão |
| Vários municípios de uma doença no legado | Contrato e recuperação de parâmetros preservados |

### Aceite

#31 já é coerente sozinho. #32 acrescenta a proteção legada sem duplicar código.
Reordenar entradas não mistura doenças. Testes de clima ausente e histórico continuam passando.

## 9. R04 — Validar INLA real e concorrência no #34

Review: https://github.com/AlertaDengue/AlertTools/pull/34
Usar também o texto atual da revisão no GitHub; o pedido inclui resultado,
tempo, memória, risco de OOM e concorrência externa por estados em host/Docker.

### Arquivos de partida e entregas previstas

- `R/nowcasting_fun.R`, `R/adjust_incidence_functions.R`, `R/core_pipeline.R`.
- `tests/testthat/test-concurrency.R`, `test-optional-models.R`.
- `tools/benchmark-concurrency.R` e `.py`.
- `tests/reference/comparison.R`, helpers e comparação do #36.
- Novo teste `test-inla-integration.R` e runner `tools/run-inla-integration.R`.
- `.github/workflows/R-CMD-check.yaml`, `docs/refactoring/concurrency.md`.
- Evidências novas em `docs/refactoring/evidence/`, sem sobrescrever as de 22/10.

### R04-A — Protocolo antes da medição

- Construir entrada controlada com notificações suficientes para executar de
  fato o ramo bayesiano, com dois ou mais municípios e múltiplas doenças.
- Confirmar os nomes efetivos dos modos aceitos; não supor que o modo `none`
  exercita INLA nem usar mocks no teste que comprova execução real.
- Fixar dados, sementes, referência, versão candidata, R, INLA e bibliotecas numéricas.
- Registrar uma política de comparação antes de rodar referência e candidata:
  colunas, chaves, tolerâncias absolutas/relativas e tratamento de NA.
- Usar repetição serial para caracterizar reprodutibilidade. Divergência acima
  da tolerância exige investigação; não ampliar tolerância apenas para aprovar.
- Definir limites de tempo/memória e tamanho da carga. Não provocar OOM no host.

### R04-B — Correção numérica e isolamento

- Comparar serial/paralelo na candidata e referência/candidata com o mesmo
  ambiente INLA, entradas e parâmetros. Declarar a adaptação de leitura usada
  pela referência; não apresentar isso como instalação completa do sistema antigo.
- Comparar casos estimados e intervalos, incidência, Rt, probabilidades,
  níveis de alerta e histórico; valores categóricos e chaves devem coincidir.
- Separar ajuste multithread da amostragem posterior, que usa `1:1` com semente.
- Verificar que grupos não compartilham séries, sementes ou parâmetros por engano
  e que opções globais do INLA não são alteradas pelo pacote.
- Se houver diferenças nos níveis de alerta, falhar e explicar a causa,
  mesmo quando diferenças numéricas pequenas estiverem dentro da tolerância.

### R04-C — Tempo, memória e comportamento operacional

- Medir inicialmente a matriz `estados externos = 1/2`,
  `municipality_workers = 1/2`, `inla_threads = 1/2`, com três repetições.
  Ajustar o tamanho da carga ao orçamento e registrar qualquer configuração não executada.
- Executar baseline e matriz no host e em Docker com limites explícitos de CPU
  e memória. Comparar cada ambiente com seu próprio baseline, sem confundir
  diferenças de infraestrutura com mudanças do pacote.
- Registrar tempo, exit code, timeout, árvore de processos, RSS agregado amostrado
  e, em Docker/cgroup, pico do grupo e ocorrência de OOM quando disponíveis.
  Explicitar limitações do RSS e evitar alegar que um teste pequeno elimina risco de OOM.
- Exercitar falha controlada de um município e recuperação após falha estadual;
  verificar subprocessos encerrados, ausência de resultados parciais tratados
  como completos e consistência de `.RData`, histórico e SQL.
- Não aumentar defaults 1/1. Recomendar orçamento externo com base nos resultados,
  deixando a escolha operacional para o consumidor.

### R04-D — CI e evidências

- Criar job Linux reduzido com INLA real, dependência explícita e timeout.
  Falha ao instalar ou executar INLA deve falhar o job; não transformar em skip verde.
- Testes locais gerais podem continuar opcionais sem INLA; o runner dedicado
  deve confirmar que os cenários obrigatórios realmente foram executados.
- Publicar artefatos de diagnóstico mesmo em falha. Benchmark amplo pode rodar
  separadamente do CI por PR; anexar seu protocolo e resultados à revisão.
- Relatório deve conter SHAs, hashes das entradas, versões, sementes,
  configuração de recursos, CSV das comparações e medições por repetição.

### Aceite

Há evidência de INLA real, equivalência dentro do protocolo, alertas coerentes,
recuperação testada e medições de host/Docker identificadas. Se faltar Docker,
dados representativos ou recursos, marcar essa parte pendente e continuar os
demais itens; não declarar migração pronta.

## 10. R05 — Auditoria Zika em homologação

Pedido: revisão do #33, que condiciona a migração à auditoria em homologação.

### Pré-requisitos e ações

- Identificar a lista de municípios realmente pretendida para a migração.
- Obter acesso de leitura ao banco de homologação pelo mecanismo de credenciais
  existente, sem imprimir, salvar no repositório ou enviar segredos nos relatórios.
- Usar `tools/audit-zika-coverage.R` com a candidata identificada, transação
  somente leitura e entradas/saídas em diretório seguro.
- Confirmar o comportamento de A92.8 específico na API nova e A90 no wrapper
  legado; parâmetros explícitos do consumidor continuam respeitados.
- Contabilizar `specific`, `legacy_only`, `missing`, `duplicate`, listar ações
  necessárias e identificar quem mantém os parâmetros. Não alterar esses dados.
- Publicar somente evidência agregada ou dados permitidos; informar o local
  protegido do relatório completo quando não puder ser versionado.

### Aceite

Cobertura medida no recorte real e identificada por data, ambiente e revisão.
Ausências e duplicidades não são mascaradas. Troca da política legada depende
da resolução dos dados e da avaliação responsável, não só de testes sintéticos.
Sem acesso ou lista de municípios: `Pendente de ambiente externo`, com requisito preciso.

## 11. R06 — Runbook de rebase e preservação dos PRs aprovados

Executar por um integrador, após preparar correções e backups. Não aplicar uma
receita de rebase à cadeia inteira sem examinar os limites de cada série.

1. Reconstruir #28 a partir da ponta remota que contém o merge #29. Incorporar
   a base candidata atual e R01. Se a candidata não avançou, evitar rebase sem efeito.
2. Reconstruir #30 sobre o novo #28, reaplicando somente sua alteração de
   depreciação e R02. O commit original de #29 na série antiga não deve reaparecer
   como alteração nova: sua implementação já está no novo pai.
3. Reconstruir #31 sobre #30; reaplicar normalização e R03-A.
4. Reconstruir #32 sobre #31; retirar duplicações de R03-A e aplicar R03-B.
5. Reaplicar #33 sobre #32, preservando a política Zika e seu teste de data.
6. Reaplicar #34 sobre #33 e acrescentar R04.
7. Reaplicar #35 sobre #34 sem alterar seu contrato aprovado de histórico mínimo.
8. Reaplicar #36 sobre #35, preservando snapshots, integridade LF/CRLF e correção
   de IDs legados. Acrescentar documentação de evidência bayesiana quando produzida.
9. Reaplicar #37 sobre #36. Remover apenas a implementação já movida para #28;
   preservar testes integrados, precisão SQL, replay PostgreSQL e artefatos.

Em cada passo:

- Analisar `git log`, merge-base e `git diff <base>...<branch>`.
- Usar limites antigos registrados em R00 para `rebase --onto`, ou reaplicar
  commits selecionados quando a transferência parcial exigir reconstrução.
  Nunca usar `--skip` apenas porque um conflito é difícil.
- Comparar séries com `git range-diff`; para merges/transferências parciais,
  complementar com diff de árvores e testes dos contratos envolvidos.
- Registrar a movimentação de cada trecho #37→#28 e #32→#31 no manifesto.
- Conferir que o diff próprio de cada PR não traz todos os commits do #20.

Publicação:

- Verificar novamente as pontas remotas antes de publicar.
- Usar lease associado ao SHA remoto observado, nunca `push --force` irrestrito.
  Se o lease falhar, reconciliar o trabalho novo; não sobrescrever.
- Publicar na ordem dos pais para os filhos, mantendo as bases coerentes.
- Não abrir PR novo para substituir um existente e não fechar #35–37.
- Quando houver merge futuro, ajustar a base dos descendentes para a branch
  que recebeu aquele merge. Não apontar tudo para `master` enquanto #20 estiver aberto.
- Revisões podem ser invalidadas por novas pontas; não declarar a aprovação
  histórica dos #35–37 como aprovação automática do novo conteúdo.

### Aceite

Todos os commits esperados presentes; nenhum merge perdido; nenhuma alteração
alheia sobrescrita. Matriz de PRs atualizada com base, head, diff e justificativa.

## 12. R07 — Validação final e evidência por revisão

Os comandos abaixo são pontos de entrada, a executar na branch apropriada e
somente depois de conferir seus pré-requisitos. Guardar saídas por SHA.

```sh
# R01/R02 e fronteira legada
Rscript -e 'devtools::test(filter = "api", stop_on_failure = TRUE)'

# R03, além dos testes novos específicos
Rscript -e 'devtools::test(filter = "core-pipeline", stop_on_failure = TRUE)'

# R04: inclui runner dedicado sem skips; nome definido na entrega
Rscript tools/run-inla-integration.R /tmp/alerttools-review-inla-evidence

# Comparações e integração já existentes na ponta da cadeia
Rscript tools/compare-epidemiology.R /tmp/alerttools-review-epidemiology
Rscript -e 'devtools::test(filter = "consumer-integration", stop_on_failure = TRUE)'

# Executar somente em PostgreSQL descartável alerttools_test, com PG* configurados
ALERTTOOLS_TEST_POSTGRES=true PGDATABASE=alerttools_test \
  Rscript tools/run-postgres-integration.R /tmp/alerttools-review-consumer-evidence

# Suíte completa após consolidação
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

- Se testes novos forem separados de `test-api.R`, acrescentar seus filtros aos
  comandos de R01/R02/R03 no relatório; não supor que o filtro acima os cobre.
- RPostgres/PostgreSQL devem usar apenas banco de teste dedicado: o helper recria schemas.
- Executar documentação e check do pacote conforme o workflow existente.
  Não chamar um check com vignettes desabilitadas de equivalente ao CI completo.
- Verificar todos os jobs dos PRs alterados no SHA publicado, incluindo o novo
  job INLA. Separar falha de infraestrutura de falha de código, sem ignorar nenhuma.
- Não atribuir equivalência bayesiana aos relatórios antigos feitos no modo `none`.

### Aceite

Testes pertinentes aprovados; testes obrigatórios de PostgreSQL e INLA não ignorados;
diffs dentro do escopo; CI verde nas novas pontas. Pendências de homologação e
validação científica registradas separadamente do aceite de código.

## 13. R08 — Matriz de atendimento e passagem para revisão

Entregar relatório final com uma linha para cada pedido, usando este formato:

| Pedido / fonte | PR destino | Commit | Teste / evidência | Resultado | Pendência |
|---|---|---|---|---|---|
| Perda de parâmetros no fallback (#28) | #28 | Preencher | Preencher | Pendente | — |
| Avisos em 1.3.0 (#30) | #30 | Preencher | Preencher | Pendente | — |
| Política aprovada no guia (#30) | #30 | Preencher | Preencher | Pendente | — |
| Município/doença em todo o cálculo (#31) | #31 | Preencher | Preencher | Pendente | — |
| Correspondência exata de pares (#31) | #31 | Preencher | Preencher | Pendente | — |
| Rejeição de múltiplas doenças no legado (#32) | #32 | Preencher | Preencher | Pendente | — |
| Auditoria Zika (#33) | #33 | Preencher | Preencher | Pendente | Ambiente/dados |
| INLA real e comparação numérica (#34/#36) | #34/#36 | Preencher | Preencher | Pendente | — |
| Concorrência, memória e tempo host/Docker (#34) | #34 | Preencher | Preencher | Pendente | Ambiente |
| Falhas e recuperação com INLA (#34/#37) | #34/#37 | Preencher | Preencher | Pendente | — |
| Atualização de bases e diffs (#28/#30–37) | Cadeia | Preencher | Range-diff/CI | Pendente | — |

Atualizar títulos/descrições ao escopo final; preparar respostas curtas com links
para commits, testes e evidências. Não marcar comentários como resolvidos apenas
por haver código semelhante em um PR posterior.

## 14. Prompt de despacho reutilizável

Copiar o bloco abaixo e substituir os campos antes de atribuir a um agente:

> Execute a tarefa [ID] de
> `docs/refactoring/plano-correcao-reviews-2026-10-29.md` no checkout [CAMINHO],
> branch [BRANCH], a partir do commit [SHA COMPLETO]. Dependências disponíveis:
> [IDs/COMMITS]. Leia a tarefa, o contrato de entrega e as restrições gerais.
> Trabalhe somente no escopo atribuído. Não faça merge, release, publicação de
> mensagens ou rebase/push de outras branches. Se houver trabalho concorrente,
> use o checkout isolado atribuído. Entregue commits, testes executados,
> evidências, pendências e uma resposta pronta ao review. Não declare aprovação
> de teste ignorado nem modifique referências históricas para acomodar a candidata.

O integrador recebe R00/R06/R08; agentes especialistas recebem os demais IDs.
Um agente pode executar a sequência inteira de forma serial se não houver
delegação. Este documento não dispara agentes nem executa suas tarefas.
