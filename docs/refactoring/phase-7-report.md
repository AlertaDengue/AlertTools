# Relatório da Fase 7 — Migração e release principal

Data: 5 de setembro de 2026

## Resultado

A infraestrutura controlada de transição foi implementada, mas o gate final da
Fase 7 permanece bloqueado por condições externas reais. Uma busca somente
leitura na organização pública `AlertaDengue` substituiu o inventário “a
confirmar” por dez grupos de consumidores; sete são críticos ou de alta
prioridade e ainda usam a API legada.

Nenhum wrapper foi removido. Após autorização expressa do mantenedor, a versão
foi preparada como 1.2.0 e a janela foi aprovada; tags, releases e publicação
continuam condicionadas à validação da candidata e dos consumidores.

## Alterações

- `consumer-registry.csv`: registro versionado de repositório, arquivo, API
  legada, criticidade, situação, evidência, bloqueio e rollback de cada grupo;
- `consumer-migration.md`: ordem de migração, baseline, transformação, testes,
  comparação epidemiológica, monitoramento e rollback por consumidor;
- `transition-policy.dcf`: sequência de versões e janela aprovada;
- `tools/check-migration-readiness.R`: valida o registro, impede regressão para
  chamadas legadas internas e bloqueia remoções prematuras;
- workflow `R-CMD-check`: novo job de integridade do inventário;
- template de PR: evidências e rollback obrigatórios por consumidor;
- README, guia de migração, vignette, contribuição, release e NEWS: comunicação
  da transição e retenção de documentação histórica.

## Consumidores confirmados

Foram encontrados usos em:

- `AlertaDengue/AlertaDengueAnalise`;
- `AlertaDengue/ADAnalise-container`;
- `AlertaDengue/AlertaDengue-migration`;
- `AlertaDengue/infodengue_intramunicipal`;
- `AlertaDengue/intramunicipal`;
- `AlertaDengue/boletins`;
- `AlertaDengue/BoletimQuinzenal`;
- `AlertaDengue/WP-estimating-Rt`.

O pipeline nacional ainda usa `pipe_infodengue()` e `tabela_historico()` e
mantém uma conexão `con` global. Outros consumidores usam `write_alerta()`,
`getCases()`, `read.parameters()` e `fouralert()`. A busca não garante cobertura
de chamadas dinâmicas, branches não indexados ou repositórios sem acesso.

## Incompatibilidades registradas

- a nova API exige conexão DBI explícita;
- leitura, cálculo, transformação e persistência agora são etapas separadas;
- o resultado moderno usa a classe `alerttools_result`;
- `version_date` é obrigatória na conversão e persistência;
- exportação de arquivo SQL, retentativas, paralelismo, `.RData` e publicação
  remota devem permanecer no orquestrador consumidor.

Essas diferenças não foram mascaradas com retorno ao estado global ou efeitos
colaterais implícitos.

## Releases de transição

A sequência `1.1.1`, `1.2.0`, `1.3.0`, `1.4.x` e `2.0.0` foi documentada. A
janela foi aprovada pelo mantenedor por no mínimo duas
releases minor e seis meses, contados a partir dos warnings formais da 1.3.0. A
`2.0.0` continua sendo somente a primeira versão elegível para remoções; não
constitui autorização automática.

## Verificações

| Verificação | Resultado |
|---|---|
| busca organizacional | 10 grupos catalogados; 7 críticos/de alta prioridade pendentes |
| chamadas legadas no código operacional de `R/` | 0 |
| integridade do registro | passou |
| gate `--for-removal` | bloqueou como esperado |
| `devtools::test()` | 258 passaram, 1 skip PostgreSQL condicionado, 0 falhas ou warnings |
| `lintr::lint_package()` | passou sem lints |
| `covr::package_coverage()` | 57,96% |
| construção local do pkgdown | passou, incluindo a vignette atualizada |
| `R CMD build` | passou, incluindo vignettes |
| `R CMD check --no-manual` | **Status: OK**, 0 errors, 0 warnings e 0 notes |

## Compatibilidade

- API: wrappers preservados; versão candidata preparada como 1.2.0;
- dados e resultados epidemiológicos: código computacional inalterado e suíte
  equivalente; consumidores externos ainda precisam de comparação própria;
- documentação histórica: política de retenção registrada.

## Gate da fase

**Não atendido ainda.** O gate exige consumidores críticos na API nova, ausência
de depreciações relevantes em operação e remoção comprovadamente segura. O
registro mostra sete consumidores críticos/de alta prioridade pendentes.

O comando abaixo materializa esse bloqueio e deve continuar falhando até que as
condições sejam realmente atendidas:

```sh
Rscript tools/check-migration-readiness.R --for-removal
```

## Decisões e autorizações necessárias

- confirmar quais consumidores são críticos e atribuir responsáveis;
- autorizar alterações, testes e PRs nos repositórios consumidores;
- fornecer infraestrutura descartável e critérios de equivalência científica;
- validar a release candidata e autorizar a principal após a migração.
