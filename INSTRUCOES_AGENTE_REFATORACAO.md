# Instruções para implementar o plano de refatoração do AlertTools

Este documento orienta um agente de desenvolvimento a executar, seção por seção, o plano definido em [`plano_refatoracao_alerttools.qmd`](plano_refatoracao_alerttools.qmd). O plano original é a fonte de verdade para objetivos, arquitetura, nova API, compatibilidade, prioridades e definição de pronto. Estas instruções convertem esse conteúdo em um fluxo operacional verificável.

## Missão do agente

Refatorar o AlertTools incrementalmente no mesmo repositório, preservando o comportamento epidemiológico validado e mantendo a API legada por meio de wrappers até a versão principal prevista. O agente deve separar acesso a dados, cálculo, orquestração e persistência sem fazer uma reescrita integral.

O trabalho deve avançar na ordem das fases 0 a 7. Não iniciar uma fase enquanto o critério de saída da fase anterior não estiver atendido ou enquanto um bloqueio não estiver explicitamente registrado e aceito pelos mantenedores.

## Regras obrigatórias de execução

1. Antes de editar, ler integralmente:
   - `plano_refatoracao_alerttools.qmd`;
   - `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore` e `README.md`;
   - os arquivos relevantes em `R/`, `tests/testthat/` e `man/`;
   - instruções locais como `AGENTS.md`, caso existam.
2. Inspecionar `git status --short` antes de cada seção e preservar alterações preexistentes do usuário.
3. Tratar cada subseção deste documento como uma unidade de trabalho. Fazer alterações pequenas, coesas e revisáveis.
4. Antes de mudar comportamento, criar ou atualizar testes de caracterização que registrem o comportamento atual relevante.
5. Não corrigir silenciosamente uma divergência epidemiológica. Classificá-la como comportamento esperado, legado ou bug conhecido e solicitar validação de domínio quando necessário.
6. Não acessar o banco de produção em testes. Usar fixtures anonimizadas, banco temporário e, quando necessário, PostgreSQL efêmero em integração contínua.
7. Não criar conexão implícita nem depender de `con`, do diretório corrente, de variáveis globais ou do estado de uma sessão interativa.
8. Não remover, renomear ou alterar de modo incompatível uma função pública legada sem a etapa de wrapper e depreciação prevista na Fase 5.
9. Não editar `NAMESPACE` ou arquivos `.Rd` manualmente quando eles forem gerados por roxygen2. Alterar a documentação no código e regenerar os artefatos.
10. Usar chamadas qualificadas, como `DBI::dbGetQuery()`, ou imports seletivos. Não usar `library()` ou `require()` em `R/`.
11. Toda leitura deve ser livre de escrita. Todo cálculo deve operar em memória. Toda persistência deve ser explícita e transacional.
12. Execução paralela deve ser opcional, portável e sequencial por padrão, com `workers = 1`.
13. Não incluir segredos, credenciais, dados identificáveis ou dumps de produção no repositório, em fixtures ou logs.
14. Não criar commits, branches, releases ou publicar artefatos sem autorização expressa.

## Ciclo obrigatório para cada seção

Para cada seção de implementação:

1. **Diagnosticar:** localizar código, testes, exports, documentação, dependências e consumidores internos afetados.
2. **Delimitar:** declarar quais arquivos e contratos serão alterados e quais ficarão fora do escopo.
3. **Proteger:** adicionar testes de caracterização ou regressão antes da mudança sempre que houver comportamento existente.
4. **Implementar:** realizar a menor alteração que cumpra a seção, mantendo compatibilidade.
5. **Documentar:** atualizar roxygen, README, vignettes, NEWS ou guia de migração conforme aplicável.
6. **Verificar:** executar primeiro testes focados, depois a suíte completa e os checks proporcionais ao impacto.
7. **Relatar:** registrar arquivos alterados, decisões, resultados dos comandos, riscos residuais e próximo passo.

Se uma verificação falhar por causa preexistente, registrar evidência que diferencie a falha anterior de uma regressão introduzida. Não declarar a seção concluída enquanto houver regressão causada pelas alterações.

## Comandos mínimos de verificação

Adaptar os comandos ao ambiente, mas preservar esta progressão:

```sh
Rscript -e 'testthat::test_file("tests/testthat/ARQUIVO_RELEVANTE.R")'
Rscript -e 'devtools::test()'
Rscript -e 'roxygen2::roxygenise()'
R CMD build .
R CMD check --no-manual AlertTools_*.tar.gz
```

Usar `devtools::document()` no lugar de `roxygen2::roxygenise()` se esse for o fluxo acordado pelo projeto. Antes de aceitar arquivos gerados, revisar o diff de `NAMESPACE` e `man/`. Não versionar diretórios `*.Rcheck`, tarballs ou outros artefatos locais de build.

## Fase 0 — Descoberta e baseline

### 0.1 Inventariar a API e os consumidores

- Listar todos os símbolos exportados pelo `NAMESPACE` e associá-los aos respectivos arquivos, assinaturas e valores de retorno.
- Procurar no repositório chamadas internas às funções públicas e referências a `AlertTools::`, `library(AlertTools)` e `require(AlertTools)`.
- Com acesso autorizado, inventariar repositórios e scripts consumidores da organização.
- Criar uma tabela versionada contendo consumidor, função usada, argumentos relevantes, colunas consumidas, criticidade e responsável.
- Não presumir que a ausência de uso no repositório significa ausência de consumidor externo.

**Entregável:** inventário da API e dos consumidores conhecidos.

### 0.2 Criar fixtures representativas

- Selecionar, com os responsáveis de domínio, municípios, doenças e períodos que cubram casos normais e limites relevantes.
- Criar fixtures anonimizadas de casos, clima, população, parâmetros e resultados.
- Incluir casos de borda: mudança de ano epidemiológico, semanas 52/53, dados ausentes, geocódigo de sete dígitos, séries curtas, duplicatas e valores extremos válidos.
- Manter fixtures pequenas, determinísticas e sem informações sensíveis.

**Entregável:** fixtures autocontidas em `tests/testthat/fixtures/` ou estrutura equivalente documentada.

### 0.3 Registrar o baseline

- Executar os fluxos atuais com as fixtures e registrar resultados de referência.
- Preferir expectativas explícitas para schemas, classes, chaves e valores críticos; usar snapshots ou golden files apenas quando forem estáveis e revisáveis.
- Registrar tolerâncias numéricas justificadas para Rt, nowcasting e MEM.
- Classificar cada resultado como esperado, legado ou bug conhecido.
- Registrar também o baseline de `R CMD check`, incluindo errors, warnings e notes.

**Gate da Fase 0:** comportamentos relevantes reproduzíveis sem banco de produção e consumidores conhecidos catalogados. Se faltarem validação de domínio, licença ou acesso a consumidores, registrar o bloqueio e solicitar decisão aos mantenedores.

## Fase 1 — Saneamento do pacote

### 1.1 Corrigir metadados e higiene do repositório

- Atualizar título, descrição, autores, URLs e `BugReports` em `DESCRIPTION` com dados confirmados.
- Não decidir a licença tecnicamente. Solicitar confirmação dos mantenedores antes de alinhar `DESCRIPTION`, `LICENSE` e documentação.
- Remover `.Rproj.user` do versionamento e garantir sua exclusão futura.
- Excluir do versionamento apenas artefatos gerados confirmados, como `*.Rcheck` e tarballs, preservando arquivos do usuário que não pertençam à tarefa.
- Adicionar `Config/testthat/edition: 3`.
- Criar `NEWS.md`, `CONTRIBUTING.md` e, se aprovado, código de conduta.

### 1.2 Corrigir dependências e namespace

- Mapear cada chamada externa para `Imports`, `Suggests` ou remoção.
- Substituir o metapacote `tidyverse` por dependências específicas quando aplicável.
- Mover dependências opcionais e não-CRAN para `Suggests`, protegendo seu uso com verificação explícita.
- Remover `library()` e `require()` de `R/`.
- Qualificar chamadas ou adicionar `@importFrom` seletivo.
- Investigar e corrigir referências inválidas, incluindo `forecast::fitted`, sem mudar o resultado validado sem teste.

### 1.3 Reparar documentação e dados

- Corrigir tags roxygen para que argumentos documentados coincidam com as assinaturas.
- Tornar exemplos executáveis, autocontidos e independentes de `con`.
- Documentar datasets públicos ou internalizá-los quando não fizerem parte da API.
- Remover texto de template da documentação do pacote.
- Regenerar `NAMESPACE` e `man/` e revisar o diff.

### 1.4 Configurar integração contínua

- Adicionar workflow de `R CMD check` para os sistemas operacionais e versões R acordados.
- Separar dependências opcionais quando necessário para que a matriz permaneça confiável.
- Não mascarar warnings ou notes apenas para deixar a CI verde; corrigir a causa ou documentar uma exceção aceita.

**Gate da Fase 1:** `R CMD check` termina com 0 errors, 0 warnings e 0 notes nos ambientes suportados. A confirmação jurídica da licença é um gate externo obrigatório para alterações de licença.

## Fase 2 — Infraestrutura de testes

### 2.1 Reorganizar a suíte

- Dividir testes por responsabilidade, por exemplo `test-epiweek.R`, `test-alert-rules.R`, `test-fetch-cases.R` e `test-pipeline.R`.
- Colocar toda execução e expectativa dentro de `test_that()`.
- Remover `setwd()`, caminhos absolutos, `library()`, `require()` e dependência de objetos criados na sessão.
- Criar helpers apenas para preparação reutilizável; não esconder a intenção dos testes em abstrações excessivas.

### 2.2 Cobrir contratos

- Testar nomes e tipos de colunas, classes, chaves, ordenação, cardinalidade, valores ausentes e erros de validação.
- Separar testes unitários, de contrato, de integração, opcionais e de regressão.
- Testar explicitamente equivalência do comportamento protegido pelo baseline.
- Para funções estocásticas, fixar `seed` e documentar tolerâncias.

### 2.3 Isolar infraestrutura

- Usar um backend temporário para testes rápidos e PostgreSQL efêmero para semântica específica do banco.
- Criar e destruir schemas de teste de modo isolado, garantindo cleanup mesmo após falhas.
- Proteger testes de INLA, MEM e outras dependências opcionais com `skip_if_not_installed()` e condições documentadas.
- Medir cobertura como indicador; não perseguir percentual às custas de testes sem valor.

**Gate da Fase 2:** suíte determinística executável por qualquer colaborador e pela CI, sem banco de produção, diretório corrente especial ou preparação manual da sessão.

## Fase 3 — Camada de acesso a dados

### 3.1 Definir a fronteira de dados

- Criar funções internas de repositório para casos, registros individuais, clima, municípios, regiões, população, parâmetros e estações.
- Traduzir tabelas e colunas físicas para schemas de domínio estáveis dentro dessa camada.
- Centralizar configuração de schemas e tabelas sem codificar credenciais.
- Fazer `conn` obrigatório em toda função que acesse banco.

### 3.2 Tornar SQL seguro e portável

- Substituir interpolação por parâmetros vinculados.
- Usar `DBI::dbQuoteIdentifier()` para identificadores e `DBI::dbQuoteLiteral()` somente quando binding não for aplicável.
- Usar `DBI::dbGetQuery()` para leitura e `DBI::dbExecute()` para escrita.
- Evitar branches espalhados por classe concreta da conexão; encapsular diferenças inevitáveis do backend.
- Validar entradas antes de executar queries, incluindo geocódigos e intervalos de datas.

### 3.3 Tornar escrita atômica e eficiente

- Substituir inserts e updates linha a linha por operações em lote.
- Executar escrita em transação, com commit somente após sucesso completo e rollback garantido em falhas.
- Tornar a política de conflito explícita: `error`, `update` ou `ignore`.
- Testar falha parcial, rollback, idempotência e conflito.

### 3.4 Remover estado global

- Localizar todas as referências a `con`, inclusive valores default e chamadas internas que deixam de propagar a conexão.
- Propagar `conn` apenas pela camada que precisa de banco; remover conexão das funções computacionais.
- Adicionar testes que falhem se uma função depender de `con` no ambiente global.

**Gate da Fase 3:** nenhum valor do usuário é concatenado diretamente em SQL; não há dependência de `con` global; toda operação de banco tem teste de integração; escrita é em lote, transacional e usa a função DBI apropriada.

## Fase 4 — Núcleo epidemiológico puro

### 4.1 Implementar calendário epidemiológico algorítmico

- Formalizar com os especialistas a convenção epidemiológica usada pelo projeto.
- Implementar `as_epiweek()`, `epiweek_start()` e `epiweek_seq()` sem limite anual fixo.
- Usar a tabela `SE` existente temporariamente como fixture de equivalência.
- Testar viradas de ano, anos com semana 53, datas-limite e round trips.

### 4.2 Extrair módulos computacionais

- Separar normalização, incidência, nowcasting, Rt, MEM e classificação de alertas.
- Fazer cada função receber objetos em memória e retornar objetos em memória.
- Não permitir chamadas a DBI, leitura ou escrita de arquivos, `setwd()` ou objetos globais nesses módulos.
- Padronizar validações e mensagens de erro sem ocultar dados inválidos.

### 4.3 Remover efeitos colaterais

- Remover a escrita e leitura implícitas de `caselist.RData`.
- Fazer datas de referência, `seed`, `workers` e opções de verbosidade argumentos explícitos.
- Usar `workers = 1` por padrão, validar valores e fornecer fallback portável.
- Não alterar planos globais de paralelismo.

### 4.4 Criar o objeto de resultado

- Implementar a classe S3 `alerttools_result` com `data`, `alerts`, `parameters`, `metadata` e `diagnostics`.
- Validar invariantes no construtor.
- Implementar `print()`, `summary()` e `as.data.frame()`; implementar `plot()` apenas se houver requisito claro.
- Testar classe, estrutura, métodos e comportamento em entradas vazias ou inválidas.

**Gate da Fase 4:** o pipeline computacional completo roda com fixtures em memória, de forma determinística, sem banco nem acesso ao diretório corrente.

## Fase 5 — Nova API e compatibilidade

### 5.1 Implementar a API em `snake_case`

- Implementar as funções propostas no plano, começando pelas necessárias ao fluxo mínimo: `new_alert_inputs()`, `fetch_alert_inputs()`, `fetch_alert_parameters()`, `run_alert_pipeline()` e `write_alert_results()`.
- Em seguida implementar as funções específicas do mapeamento da API atual conforme necessidade comprovada.
- Usar consistentemente `conn`, `geocodes`, `start_date`, `end_date`, `start_week`, `report_week`, `disease`, `workers` e `verbose`.
- Documentar schema, classe, chaves e ordenação de todas as entradas e saídas públicas.
- Marcar funções ainda sujeitas a mudança como experimentais.

### 5.2 Preservar a API legada

- Converter cada função antiga em wrapper fino da nova API sempre que a equivalência for comprovada.
- Emitir aviso com `lifecycle::deprecate_warn()` e indicar a substituição.
- Preservar assinatura, defaults e forma de saída legada durante a janela de compatibilidade, adaptando internamente quando necessário.
- Não manter duas implementações independentes da mesma regra.

### 5.3 Criar e testar a migração

- Criar tabela de migração com função antiga/nova, argumentos, colunas, diferenças e versão de depreciação.
- Testar equivalência entre APIs para os cenários do baseline.
- Adicionar testes específicos dos wrappers, inclusive warnings e adaptação de argumentos.

**Gate da Fase 5:** a nova API reproduz os resultados aprovados, a API antiga continua funcional por wrappers e as diferenças intencionais estão documentadas.

## Fase 6 — Documentação e experiência de desenvolvimento

### 6.1 Documentar o uso

- Atualizar o README com instalação, requisitos e exemplo mínimo executável.
- Criar uma vignette do pipeline com banco e outra usando apenas dados em memória.
- Criar guia de migração da API antiga para a nova.
- Garantir que exemplos não dependam de credenciais, produção ou estado global.

### 6.2 Documentar manutenção e release

- Registrar como instalar dependências opcionais, executar testes, atualizar documentação e rodar checks.
- Documentar versionamento, depreciação, release candidata e release principal.
- Manter `NEWS.md` com alterações voltadas ao usuário.

### 6.3 Automatizar qualidade e publicação

- Configurar pkgdown e publicar somente após validação dos mantenedores.
- Adicionar badges de check e cobertura quando os respectivos serviços existirem.
- Configurar lintr e styler com regras aprovadas; separar mudanças puramente de estilo das mudanças funcionais.
- Configurar proteção do branch e revisão obrigatória apenas com autorização administrativa.

**Gate da Fase 6:** uma pessoa nova consegue instalar, testar e executar o exemplo em memória seguindo apenas a documentação publicada.

## Fase 7 — Migração e release principal

### 7.1 Migrar consumidores

- Atualizar cada consumidor catalogado, um por vez, com testes e plano de rollback próprios.
- Monitorar avisos de depreciação e registrar incompatibilidades reais.
- Corrigir problemas na nova API sem reintroduzir dependência global ou efeitos colaterais.

### 7.2 Conduzir releases de transição

- Seguir, salvo decisão posterior, a sequência: `1.1.1` para saneamento; `1.2.0` para camada de dados e API experimental; `1.3.0` para API estável e warnings; `1.4.x` para transição; `2.0.0` para remoções aprovadas.
- Publicar release candidata antes da versão principal.
- Definir e comunicar a janela de suporte da API antiga.

### 7.3 Remover a API antiga com segurança

- Remover wrappers somente após confirmação de que consumidores críticos migraram e de que a janela publicada terminou.
- Registrar cada remoção no `NEWS.md` e no guia de migração.
- Arquivar a documentação anterior sem apagar o histórico necessário aos usuários de versões antigas.

**Gate da Fase 7:** consumidores críticos usam a nova API, não há depreciações relevantes em operação e a retirada dos wrappers não interrompe pipelines conhecidos.

## Ordem de prioridade dentro das fases

Quando houver escolha de tarefas, executar nesta ordem:

1. **P0:** fixtures e testes de caracterização; `R CMD check` limpo; remoção de `con`; SQL parametrizado; remoção de arquivos temporários implícitos.
2. **P1:** núcleo puro; nova API e wrappers; CI multiplataforma.
3. **P2:** pkgdown, cobertura, lint e otimizações de desempenho.

Não antecipar otimizações antes de estabilizar contratos e testes. Toda otimização deve incluir benchmark reproduzível e teste de equivalência.

## Decisões que exigem participação humana

O agente deve pausar o ponto afetado, continuar apenas em tarefas independentes e solicitar decisão quando houver:

- confirmação ou mudança de licença;
- definição da convenção epidemiológica ou validação de resultados científicos;
- escolha de consumidores críticos e janela de depreciação;
- acesso a repositórios privados, dados, banco ou credenciais;
- alteração de infraestrutura externa, proteção de branch, publicação de site ou release;
- incompatibilidade que obrigue escolher entre preservar o legado e corrigir um bug.

Na solicitação, apresentar evidências, opções, impacto e recomendação. Não escolher silenciosamente em nome dos mantenedores.

## Formato do relatório ao concluir cada seção

Usar este modelo:

```md
## Seção concluída: <fase e título>

### Resultado
<o que passou a funcionar>

### Alterações
- <arquivo ou componente>: <mudança>

### Verificações
- `<comando>`: passou/falhou
- Resultado do check: <errors, warnings, notes>

### Compatibilidade
- API: preservada/alterada
- Dados e resultados epidemiológicos: equivalentes/diferença aprovada

### Pendências e riscos
- <pendência, responsável e condição de desbloqueio>

### Próxima seção recomendada
<uma única seção, sem iniciá-la automaticamente se depender de aprovação>
```

## Checklist final da refatoração

A tarefa completa somente pode ser declarada pronta quando todos os itens abaixo forem verdadeiros:

- [ ] `R CMD check` tem 0 errors, 0 warnings e 0 notes.
- [ ] A CI cobre os sistemas e versões R acordados.
- [ ] Nenhuma função depende do objeto global `con`.
- [ ] Nenhuma função de cálculo acessa banco, arquivos ou diretório corrente.
- [ ] Todo SQL usa binding ou quoting seguro.
- [ ] Escritas são explícitas, em lote e transacionais.
- [ ] O núcleo executa integralmente com fixtures em memória.
- [ ] Schemas públicos estão documentados e testados.
- [ ] Paralelismo é explícito, portável e sequencial por padrão.
- [ ] A API antiga tem wrappers ou foi removida conforme a política publicada.
- [ ] Consumidores críticos foram migrados.
- [ ] README, vignettes, NEWS e guia de migração estão atualizados.
- [ ] Resultados epidemiológicos foram aprovados pelos responsáveis de domínio.
- [ ] Licença e metadados foram confirmados pelos mantenedores.

## Instrução de início para o agente

Comece sempre pela primeira seção ainda não concluída. Apresente o diagnóstico e o escopo da seção antes de editar. Ao final, execute as verificações aplicáveis, compare o resultado com o baseline e produza o relatório padronizado. Não avance para a seção seguinte se o gate atual não estiver satisfeito.
