# Relatório da Fase 1 — Saneamento do pacote

Data: 20 de agosto de 2026

## Resultado

O pacote pode ser construído, instalado, carregado, documentado e verificado
sem errors, warnings ou notes no ambiente local de referência.

## Alterações

- `DESCRIPTION`: metadados atualizados; dependências específicas substituem o
  metapacote `tidyverse`; dependências opcionais estão em `Suggests`;
  `testthat` edition 3, URL e rastreador de issues foram declarados.
- `NAMESPACE` e `man/`: regenerados com roxygen2 a partir do código-fonte.
- `R/`: removidas chamadas ativas a `library()` e `require()`; chamadas
  externas passaram a ser qualificadas ou importadas seletivamente.
- Dependências opcionais `brpop`, `INLA`, `mem` e `data.table` agora são
  verificadas explicitamente antes do uso.
- A referência inválida a `forecast::fitted()` foi substituída por
  `stats::fitted()`.
- Datasets públicos foram documentados. Os três datasets históricos com
  codificação inválida (`bairro2APS`, `locs` e `sinan`) foram excluídos do
  artefato de build, mas preservados no repositório para avaliação posterior.
- O diretório RStudio `AlertTools/.Rproj.user` e a `.Rbuildignore` aninhada
  foram removidos.
- Foram adicionados `NEWS.md`, `CONTRIBUTING.md` e um workflow multiplataforma
  de `R CMD check`.
- Testes legados que dependem de banco foram transformados em skips explícitos;
  o baseline em memória permanece ativo.

## Verificações

| Verificação | Resultado |
|---|---|
| `devtools::document()` | passou |
| `devtools::test()` | 82 passaram, 3 skips explícitos, 0 falhas |
| `R CMD build` | passou |
| `R CMD check --no-manual` | **Status: OK** |

O check foi executado com R 4.4.3 em macOS arm64. O ambiente não conseguiu
consultar índices remotos durante o check, mas todas as dependências sugeridas
estavam instaladas e a verificação completa terminou com sucesso.

## Compatibilidade

- Nenhum símbolo foi removido do `NAMESPACE`.
- Assinaturas públicas foram preservadas.
- Os 28 testes de caracterização da Fase 0 continuam passando.
- Três integrações dependentes de banco permanecem sem execução até a criação
  da infraestrutura temporária prevista na Fase 2.

## Licença

O campo `License` passou a apontar para `file LICENSE`, tornando o arquivo GPL-3
já presente no repositório a fonte textual do build sem inferir se a intenção
dos mantenedores era substituir `CCO` por `GPL-3`. A confirmação jurídica da
licença continua obrigatória antes de uma publicação.

## Pendências e riscos

- Executar o workflow recém-criado no GitHub Actions; ele não pode ser validado
  integralmente apenas no ambiente local.
- Confirmar a licença pretendida e a compatibilidade das contribuições.
- Decidir se os datasets históricos excluídos do build devem ser recodificados,
  internalizados ou removidos em versão futura.
- Substituir os skips de banco por testes com backend temporário na Fase 2.

## Próxima seção recomendada

Fase 2 — Infraestrutura de testes.
