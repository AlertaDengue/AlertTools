# Relatório da Fase 4 — Núcleo epidemiológico puro

Data: 4 de setembro de 2026

## Resultado

O fluxo computacional completo pode ser executado por `alerttools_pipeline()`
somente com objetos em memória. O resultado é determinístico quando o
nowcasting é desativado ou recebe uma semente, não consulta banco, não lê ou
grava arquivos e não depende do diretório corrente.

## Calendário epidemiológico

Foram implementadas três funções algorítmicas:

- `as_epiweek()`: converte datas em semanas `YYYYWW`;
- `epiweek_start()`: retorna o domingo inicial da semana;
- `epiweek_seq()`: produz sequências inclusivas entre semanas.

A convenção preservada é domingo a sábado, com a semana 1 contendo 4 de
janeiro. O algoritmo não possui tabela ou limite anual. A tabela legada `SE`
foi usada como fixture de equivalência. Ela contém uma anomalia isolada na
semana 201815, registrada com início em uma quarta-feira e duração de dez dias;
o novo algoritmo retorna corretamente 8 a 14 de abril de 2018.

Os testes cobrem viradas de ano, anos com 53 semanas, intervalos entre 1900 e
2101, uma data em 2150, round trips, valores ausentes e semanas inválidas.
As funções legadas `data2SE()`, `SE2date()`, `episem()`, `daySEday()`,
`lastepiweek()` e `seqSE()` agora delegam à convenção algorítmica.

## Módulos computacionais

O novo núcleo coordena operações já separadas e puras para:

- validação e normalização dos schemas de domínio;
- incidência por `calculate_incidence()`;
- correção de atraso por `adjustIncidence()` e `bayesnowcasting()`;
- número reprodutivo por `Rt()`;
- limiares MEM por `applymem()`;
- critérios e classificação por `setCriteria()` e `fouralert()`.

Essas entradas computacionais recebem data frames ou vetores em memória. Um
teste estrutural protege o núcleo contra chamadas a DBI, leitura e escrita de
arquivos e alteração do diretório corrente. Dados inválidos, como população
não positiva, contagem negativa, schemas incompletos e número de trabalhadores
inválido, produzem erros explícitos.

## Efeitos colaterais e determinismo

`getCases()` não grava mais `caselist.RData`. Quando registros individuais são
necessários, eles são carregados pela camada de dados, anexados em memória e
passados ao núcleo pelo argumento `case_records`.

O nowcasting bayesiano não consulta mais o banco internamente. `seed`,
`workers` e `verbose` são argumentos propagados pelo pipeline. A semente cobre
tanto a amostragem posterior quanto a preditiva e o estado aleatório do
chamador é restaurado ao final. `workers = 1` é o padrão; valores são validados,
e plataformas sem `fork` usam execução serial sem alterar planos globais.

`pipe_infodengue()` exige `datarelatorio` ou `finalday` explícito. A data de
versão também é propagada explicitamente para persistência.
`tabela_historico()` e `tabela_historico_intra()` não consultam o relógio e
exigem `versao`.

## Objeto de resultado

`new_alerttools_result()` cria a classe S3 `alerttools_result` com os campos:

- `data`;
- `alerts`;
- `parameters`;
- `metadata`;
- `diagnostics`.

O construtor valida tipos, cardinalidade, nomes duplicados e listas nomeadas.
Foram implementados `print.alerttools_result()`,
`summary.alerttools_result()` e `as.data.frame.alerttools_result()`. Não foi
adicionado `plot()`, pois o plano condiciona esse método à existência de um
requisito visual claro.

## Verificações

| Verificação | Resultado |
|---|---|
| `devtools::test()` | 201 passaram, 1 skip PostgreSQL condicionado, 0 falhas |
| `covr::package_coverage()` | 54,08% |
| pipeline repetido com fixture e mesma semente | resultados idênticos |
| ausência de arquivos novos após o pipeline | confirmada em diretório temporário |
| restauração de `.Random.seed` | confirmada |
| equivalência com calendário legado válido | confirmada |
| `R CMD build` | passou |
| `R CMD check --no-manual` | **Status: OK**, sem erros, avisos ou notas |

O check foi executado com R 4.4.3 em macOS arm64. O teste PostgreSQL permanece
condicionado ao serviço efêmero da CI; os testes unitários e de integração
SQLite foram executados localmente.

## Compatibilidade e migração

As funções legadas de infraestrutura continuam disponíveis. As mudanças
deliberadas desta fase são:

- `pipe_infodengue()` retorna `alerttools_result`;
- `pipe_infodengue()` requer uma semana ou data final explícita;
- `tabela_historico*()` requer `versao` explícita;
- nowcasting bayesiano requer registros individuais em memória;
- `GenTimeDist()` usa `workers = 1`, mantendo `nc` como alias depreciado.

Nenhum símbolo exportado anterior foi removido. A camada de compatibilidade e
os nomes finais da nova API pertencem à fase 5.

## Gate da fase

O gate foi atendido: o pipeline epidemiológico completo roda com fixtures em
memória, de forma determinística, sem banco, arquivos auxiliares ou dependência
do diretório corrente.

## Próxima seção recomendada

Fase 5 — Nova API e compatibilidade.
