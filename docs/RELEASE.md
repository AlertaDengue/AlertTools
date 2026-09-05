# Processo de release do AlertTools

Este documento separa preparação técnica, validação científica e publicação.
Publicação, tags, GitHub Pages e proteção de branch exigem autorização dos
mantenedores.

## Versionamento

O projeto segue versionamento semântico:

- patch: correções compatíveis e documentação;
- minor: funcionalidades compatíveis e API experimental;
- major: remoções ou mudanças incompatíveis anunciadas.

Sequência de transição proposta pelo plano:

| Versão | Conteúdo |
|---|---|
| 1.1.1 | saneamento, documentação, dependências e CI |
| 1.2.0 | camada de dados e API experimental |
| 1.3.0 | API estável e início formal dos warnings |
| 1.4.x | migração de consumidores e correções |
| 2.0.0 | remoções previamente aprovadas |

A versão 1.2.0 foi aprovada pelo mantenedor como release candidata para reunir
o saneamento e a API experimental. A 1.1.1 não foi publicada separadamente;
essa decisão não antecipa a estabilização da API prevista para 1.3.0.

## Depreciação

1. Introduza a substituição e prove equivalência por testes.
2. Mantenha o wrapper legado e indique a substituição com `lifecycle`.
3. Registre argumentos e diferenças no guia de migração e em `NEWS.md`.
4. Monitore consumidores e estabeleça uma janela pública de suporte.
5. Remova somente em versão major aprovada, após confirmação dos consumidores
   críticos.

O registro de consumidores e a política aprovada ficam em
`docs/refactoring/consumer-registry.csv` e
`docs/refactoring/transition-policy.dcf`. Durante a transição, valide sua
integridade com:

```sh
Rscript tools/check-migration-readiness.R
```

Antes de remover qualquer wrapper, o comando abaixo precisa passar:

```sh
Rscript tools/check-migration-readiness.R --for-removal
```

Ele permanece bloqueado enquanto a janela não estiver formalmente aprovada ou
houver consumidor crítico/de alta prioridade sem validação ou aposentadoria.

## Checklist da release candidata

- [ ] branch limpo e revisão aprovada;
- [ ] versão e data confirmadas pelos mantenedores;
- [ ] documentação roxygen atualizada;
- [ ] `NEWS.md` voltado ao usuário;
- [ ] exemplos e vignettes executados sem credenciais;
- [ ] testes SQLite e PostgreSQL efêmero aprovados;
- [ ] CI aprovada em Linux, macOS, Windows e R-devel;
- [ ] `R CMD check` com 0 errors, warnings e notes;
- [ ] lint aprovado e cobertura revisada como indicador;
- [ ] validação científica registrada quando necessária;
- [ ] pacote fonte e site construídos como artefatos;
- [ ] plano de rollback definido.
- [ ] registro de consumidores atualizado e warnings de depreciação revisados;
- [ ] política de suporte aprovada, se a release alterar depreciações.

Crie uma tag candidata, por exemplo `v1.2.0-rc.1`, e disponibilize o pacote
fonte para validação. Não publique a versão principal antes do aceite da RC.

## Release principal

Após aceite da candidata:

1. atualize versão, data e `NEWS.md`;
2. execute novamente toda a matriz de qualidade;
3. gere o pacote fonte a partir do commit aprovado;
4. crie tag assinada e release com notas de migração;
5. publique o site pelo workflow manual `pkgdown`, digitando `publish`;
6. monitore instalação, warnings e pipelines consumidores;
7. reverta a tag/release ou publique correção patch conforme o plano de
   rollback se houver regressão.

Uma release `2.0.0` não está autorizada apenas por atingir o número de versão:
ela também exige aprovação explícita da janela e gate de remoção verde. Tags e
releases candidatas não são criadas automaticamente por este repositório.

## Artefatos e evidências

Anexe à release:

- tarball produzido por `R CMD build`;
- resultados de check;
- cobertura em `coverage.rds` gerada pela CI;
- link para documentação publicada;
- decisões científicas e incompatibilidades aprovadas.
