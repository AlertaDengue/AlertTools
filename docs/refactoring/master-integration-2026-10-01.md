# Integração do master — 01/10/2026

## Entrega

A branch `codex/alerttools-1.2.0-rc1`, vinculada ao PR #20, foi atualizada até
`origin/master` (`e460b3b`). O merge local pendente do PR #19 foi concluído,
preservando a refatoração e a remoção do campo tweet. O histórico publicado foi
preservado por merge, sem rebase ou force push.

Commits publicados: `84158ba` e `aaf7ab2`.

- PR #22: `firstday` foi integrado ao wrapper refatorado e encaminhado a
  `fetch_cases(start_date = firstday)`, com padrão `2018-01-01`. Use o argumento
  pelo nome; ele foi acrescentado após os argumentos da release candidata para
  preservar as posições existentes. `iniSE` continua delimitando o clima.
- PR #24: ambos os objetos SE corrigidos foram incorporados. A regressão das
  semanas 201814–201816, continuidade, duração, correspondência com `episem()` e
  equivalência entre os calendários público e interno foi preservada.
- Os testes antigos substituídos na refatoração não foram reintroduzidos.
  O novo teste de histórico usa parâmetros e data de versão explícitos,
  sem sobrescrever bindings bloqueados ou carregar fontes no ambiente de testes.
- O workflow já existente no PR #20 foi mantido. A instalação explícita de
  `devtools` foi adicionada ao job PostgreSQL, que executa `devtools::test()`.

## Evidências

- Suíte completa: **297 expectativas aprovadas, 0 falhas, 0 warnings, 1 skip**.
  O skip é a integração PostgreSQL opt-in, que exige banco efêmero.
- R CMD build completo, incluindo vignettes: aprovado, utilizando o Pandoc
  instalado com RStudio via RSTUDIO_PANDOC.
- R CMD check --no-manual, com _R_CHECK_FORCE_SUGGESTS_=false: **0 erros,
  0 warnings, 1 NOTE**, exclusivamente pela ausência local de `lintr`.
- Lint não executado: pacote `lintr` indisponível localmente.
- Workflow YAML: leitura sintática aprovada; isso não confirma execução remota.
- Registro de migração: íntegro; remoção de APIs legadas continua bloqueada
  pelos sete consumidores críticos/altos ainda pendentes, conforme política.
- `git diff --check`: aprovado. `origin/master` é ancestral da branch publicada.
- A consulta inicial do conector não retornou execuções. A API pública
  confirmou depois que o CI executou e falhou ao resolver a dependência INLA.
  O repositório adicional já declarado no DESCRIPTION foi incluído explicitamente
  em setup-r para os jobs que instalam dependências. A nova execução será
  acompanhada antes de registrar aprovação remota.

Os arquivos locais `plano_refatoracao_alerttools.html` e
`plano_refatoracao_alerttools.qmd` foram preservados sem alterações.

## Issue #25: Enable automated package checks and regression tests on master

https://github.com/AlertaDengue/AlertTools/issues/25

Após #22 e #24, a equipe pediu execução automática de testes no AlertTools.
O master ainda não contém R-CMD-check. O PR #20 já introduz checks de pacote
em Linux/macOS/Windows, PostgreSQL efêmero, cobertura, lint e validação do
registro de migração. Acompanhar a entrega por esse PR evita implementação
duplicada. Relacionar à issue de falhas de testes abaixo quando criada.

Critérios de aceite:

- [ ] Executar em pull requests e pushes para master.
- [ ] Instalar dependências em runners limpos.
- [ ] Executar a suíte completa, incluindo calendário e firstday.
- [ ] Instalar devtools explicitamente no job PostgreSQL.
- [ ] Expor falhas e logs no GitHub Actions.
- [ ] Registrar execução remota aprovada antes de entregar ao master.

## Issue #26: Deliver fixes for the three preexisting test failures reported in PR #24

https://github.com/AlertaDengue/AlertTools/issues/26

O comentário de validação de #24 registra três falhas anteriores à correção:
`assert_that` ausente em test_alertfunctions.R, `con` ausente em
test_timeseriespipeline.R e binding bloqueado de `read.parameters` no teste
histórico. As duas primeiras já foram tratadas pelo PR #20 com fixtures,
imports e conexões explícitas. A integração do teste histórico agora elimina
mutação do namespace e carregamento direto de fontes. Relacionar à issue de CI
acima quando criada.

Critérios de aceite:

- [ ] Entregar ao master uma suíte completa sem falhas ou warnings.
- [ ] Exercitar o pacote carregado sem sobrescrever bindings bloqueados.
- [ ] Preservar regressões do calendário e de firstday.
- [ ] Confirmar os resultados em CI antes da entrega.

## Pendências externas

A criação de issue pelo conector retornou HTTP 403 (Resource not accessible
by integration). A autenticação local do gh também está inválida. O Git
conseguiu publicar a branch, mas a revisão automática bloqueou a reutilização
da credencial do Git para a API: exige autorização específica do usuário.
Após autorização específica do usuário, as issues #25 e #26 foram criadas
utilizando a autenticação do Git, sem exibir ou salvar o token. A edição da
descrição do PR pelo conector também retornou 403; ela ainda contém os números
da validação anterior. Este relatório registra os números atuais. Não houve
merge do PR #20 no master.
