# Relatório da Fase 6 — Documentação e experiência de desenvolvimento

Data: 5 de setembro de 2026

## Resultado

O pacote agora possui uma trilha documentada e testada para instalação,
desenvolvimento, uso em memória, uso com banco e migração da API legada. A
documentação executável não usa credenciais, produção nem estado global.

O site pkgdown e sua publicação manual foram configurados, mas não publicados.
A publicação e os controles administrativos permanecem deliberadamente
dependentes da validação e autorização dos mantenedores.

## Uso e onboarding

O `README.md` passou a documentar:

- requisitos e instalação da versão de desenvolvimento;
- preparação de um clone para desenvolvimento;
- exemplo mínimo executável sem banco;
- separação entre leitura, cálculo e escrita no fluxo com DBI;
- acesso às vignettes, migração e manutenção;
- ausência de credenciais, dados de produção e conexão global nos exemplos.

Foram adicionados dois scripts reproduzíveis em `inst/examples/`:

- `in-memory-pipeline.R`, com casos, clima e parâmetros sintéticos;
- `sqlite-pipeline.R`, com banco SQLite em memória e tabelas descartáveis.

Os scripts sustentam as vignettes `in-memory-pipeline`, `database-pipeline` e
`migrating-to-snake-case`. Um teste dedicado executa os dois exemplos em
ambientes limpos e protege seu contrato de onboarding.

## Manutenção e release

O `CONTRIBUTING.md` registra dependências opcionais, documentação, testes,
lint, cobertura, check, vignettes, site, segurança de banco e revisão de
mudanças científicas.

`docs/RELEASE.md` documenta versionamento semântico, janela de depreciação,
release candidata, release principal, artefatos, rollback e a sequência de
versões proposta. O `NEWS.md` foi atualizado com alterações voltadas a quem
usa e mantém o pacote.

## Qualidade e publicação

Foram configurados:

- site pkgdown Bootstrap 5, referência agrupada e três guias;
- workflow `pkgdown` acionado apenas manualmente, condicionado à confirmação
  textual e ao ambiente protegido `pkgdown-production`;
- badge do workflow `R-CMD-check`, que já possui serviço correspondente;
- cobertura como indicador e artefato `coverage.rds` na CI;
- job de lint na CI e regras locais focadas em caminhos absolutos, whitespace
  e alteração do diretório de trabalho;
- script opt-in do `styler`, limitado aos arquivos modernos e com orientação
  para commits exclusivamente de estilo;
- recomendações de proteção de branch e revisão obrigatória em
  `docs/BRANCH_PROTECTION.md`.

Não foi criado badge de porcentagem de cobertura porque ainda não existe um
serviço público de cobertura confirmado. Não foram publicados o site, tags ou
releases, nem alteradas configurações do GitHub.

## Verificações

| Verificação | Resultado |
|---|---|
| exemplos documentados | 8 testes passaram |
| `devtools::test()` | 258 passaram, 1 skip PostgreSQL condicionado, 0 falhas ou warnings |
| `lintr::lint_package()` | passou sem lints |
| `covr::package_coverage()` | 57,96% |
| sintaxe dos workflows e do pkgdown | YAML válido |
| construção local do pkgdown | passou, incluindo referência e 3 artigos |
| `R CMD build` | passou, incluindo construção das vignettes |
| `R CMD check --no-manual` | **Status: OK**, 0 errors, 0 warnings e 0 notes |

O check foi executado com R 4.4.3 em macOS arm64. A integração PostgreSQL
continua condicionada ao serviço efêmero configurado na CI.

## Gate da fase

O percurso técnico do gate foi atendido localmente: uma instalação construída
do pacote contém o exemplo em memória, as vignettes são construídas e o exemplo
é executado automaticamente pela suíte. A disponibilização desse percurso na
URL pública depende apenas da validação dos mantenedores e da execução manual
do workflow de publicação, como exigido pelo próprio plano.

## Decisões externas pendentes

- validar e publicar o site pkgdown;
- criar e proteger o ambiente `pkgdown-production` no GitHub;
- aprovar e aplicar proteção do branch e revisão obrigatória;
- escolher um serviço público antes de adicionar badge de cobertura;
- confirmar versão, tag e calendário da próxima release candidata.

## Próxima seção recomendada

Fase 7 — Migração e release principal.
