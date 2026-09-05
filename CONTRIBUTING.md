# Contribuindo com o AlertTools

Obrigado por contribuir. Mudanças devem ser pequenas, reproduzíveis e preservar
os contratos epidemiológicos protegidos pelos testes.

## Preparação do ambiente

Com R e Git instalados, clone o repositório e execute na raiz do pacote:

```r
install.packages("pak")
pak::pak()
devtools::load_all()
```

Dependências opcionais têm responsabilidades específicas:

- `RSQLite`: integração local descartável;
- `RPostgres`: integração PostgreSQL e uso operacional;
- `INLA`: nowcasting bayesiano;
- `mem`: limiares MEM;
- `knitr`, `rmarkdown` e `pkgdown`: documentação e site;
- `covr`, `lintr` e `styler`: cobertura e qualidade estática.

INLA usa o repositório adicional declarado no `DESCRIPTION`. Não torne INLA ou
MEM obrigatórios para quem usa somente o núcleo básico.

## Fluxo de desenvolvimento

1. Abra ou referencie uma issue com o comportamento esperado.
2. Crie um branch focado.
3. Adicione um teste de caracterização antes de mudar regra epidemiológica.
4. Implemente a menor alteração que satisfaça o contrato.
5. Atualize roxygen e `NEWS.md` quando houver efeito para usuários.
6. Execute testes, lint e check.
7. Abra um pull request descrevendo validação, riscos e impacto de migração.

## Comandos locais

```r
devtools::document()
devtools::test(stop_on_failure = TRUE)
lintr::lint_package()
devtools::check(error_on = "warning")
covr::package_coverage()
```

Para verificar as vignettes e o site:

```r
devtools::build(vignettes = TRUE)
pkgdown::build_site(preview = FALSE, new_process = TRUE)
```

Os exemplos publicados também são executados por
`tests/testthat/test-documentation-examples.R`.

Para uma mudança de migração ou depreciação:

```sh
Rscript tools/check-migration-readiness.R
```

Atualize `docs/refactoring/consumer-registry.csv` no mesmo PR, incluindo
responsável, evidência, incompatibilidades e rollback. Use o template específico
de migração em `.github/PULL_REQUEST_TEMPLATE/consumer-migration.md`.

## Estilo

Execute `Rscript tools/style.R` somente em um branch ou commit dedicado a
formatação. Revise o diff e não misture alterações puramente de estilo com
mudanças funcionais. Arquivos legados fora do escopo do script serão formatados
gradualmente, para evitar diffs extensos sem ganho funcional.

As regras automatizadas ficam em `.lintr`. Não desabilite uma regra apenas para
silenciar um caso; documente a exceção ou ajuste o código.

## Bancos e dados sensíveis

Testes não podem acessar produção, depender de `con` global ou gravar no
diretório de trabalho. Nunca registre credenciais ou dados de saúde
identificáveis.

A suíte regular cria SQLite em memória. A integração PostgreSQL exige um banco
descartável chamado exatamente `alerttools_test`, com
`ALERTTOOLS_TEST_POSTGRES=true`. O helper recusa outro nome de banco e recria
somente os schemas dedicados ao teste.

## Compatibilidade e ciência

- Não remova wrappers depreciados sem a janela de suporte aprovada.
- Migre consumidores um por PR e mantenha a versão anterior implantável.
- Nos testes do consumidor, use `options(lifecycle_verbosity = "warning")` e
  trate warnings remanescentes como evidência de migração incompleta.
- Atualize `vignettes/migrating-to-snake-case.Rmd` e a tabela de migração ao
  alterar a API.
- Mudanças em calendário, nowcasting, Rt, MEM ou classificação exigem revisão
  dos mantenedores de domínio.
- Mudanças de licença exigem decisão explícita dos mantenedores.

## Pull request

O PR deve informar:

- problema e solução;
- testes adicionados ou atualizados;
- resultado de `R CMD check`;
- mudança de API ou schema;
- risco epidemiológico e necessidade de validação humana;
- plano de migração e rollback, quando aplicável.

Consulte [docs/RELEASE.md](docs/RELEASE.md) para releases e
[docs/BRANCH_PROTECTION.md](docs/BRANCH_PROTECTION.md) para os controles que
dependem de autorização administrativa.
