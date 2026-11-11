# Comparação epidemiológica controlada

O relatório reúne duas comparações controladas: quatro cenários sem nowcasting
e um cenário bayesiano com INLA real. A validação com a carga operacional
completa permanece como requisito da migração do AlertaDengueAnalise.

## Referências e método

A revisão `27a03f26a9bf366597ef135e7da8d2f620ea9a8e` do consumidor ainda fixa
AlertTools em `e460b3b8c15ab28e8755f482d576cf7280803b34`, conforme
`scripts/install_deps.R`. As expressões das funções de cálculo e conversão
dessa dependência foram extraídas sem alterações em `tests/reference/e460b3b8`.
O manifesto registra commit, blobs originais, funções selecionadas e hashes dos
arquivos extraídos. O calendário interno foi preservado.
Os hashes dos textos R usam UTF-8/LF canônico para suportar checkouts Windows;
alterações de conteúdo continuam falhando. O arquivo binário mantém hash bruto.
A consulta histórica de parâmetros é substituída somente por uma leitura dos parâmetros controlados;
não se executa SQL antigo nem se acessa um banco operacional.

`tools/prepare-epidemiological-reference.R` regenera exclusivamente a partir do
commit fixo e verifica que as expressões R continuam idênticas às originais.
Testes e comparação nunca regeneram a referência para aceitar a candidata.

```sh
Rscript tools/compare-epidemiology.R /tmp/epidemiology-evidence
```

São comparados dados, todos os índices de alerta e todos os campos do histórico:
contagens notificadas e ajustadas, limites, incidência, Rt, intervalos,
probabilidades, níveis, clima, chaves, IDs, datas e formato persistível. A ordem
de colunas pode mudar; a correspondência é por nome. A tolerância absoluta é
`1e-7`, já utilizada no baseline anterior; valores ausentes, textos e datas são
exatos. Mudanças de esquema/cardinalidade ou valores fora da tolerância falham.

## Resultado local em 22/10/2026

| Cenário | Modo | Métricas | Divergências finais |
|---|---|---|---|
| Dengue | none | 58 | 0 |
| Chikungunya | none | 58 | 0 |
| Zika com fallback legado | none | 58 | 0 |
| Dengue com semana climática ausente e valor parcial ausente | none | 58 | 0 |

As 232 métricas coincidiram. Os CSVs e o ambiente estão em
`evidence/epidemiology-2026-10-22/`, incluindo hashes dos fontes candidatos.

A comparação inicial encontrou uma divergência no `id`: a candidata inseria o
identificador de localidade `0` onde o conversor legado concatenava um campo
ausente. O wrapper legado agora preserva o formato da referência. A API nova
mantém seu identificador explícito e a diferenciação de doenças introduzida
anteriormente. Um `alerttools_result` passado ao wrapper mantém a conversão da
API nova; a adaptação de ID é restrita aos objetos legados.

## Comparação bayesiana com INLA real

O cenário de referência do PR #36 usa séries sintéticas de dengue e chikungunya
para um município. Executa os cálculos históricos fixados em `e460b3b8` e a candidata
com o mesmo motor INLA e quatro threads de ajuste, o orçamento histórico.
O protocolo fornece a mesma semente à amostragem posterior dos dois lados
para comparar as implementações sob condições controladas.

As 58 métricas coincidiram, com tolerância absoluta de `1e-7` para valores
numéricos e correspondência exata de chaves, categorias e valores ausentes.
O runner obrigatório do PR #36 concluiu três cenários e sete verificações
sem falhas, avisos ou skips. Consulte a
[comparação por métrica](evidence/review-2026-10-29/local-pr36-inla/reference-comparison.csv),
os [resultados dos testes](evidence/review-2026-10-29/local-pr36-inla/test-results.csv)
e o [ambiente de execução](evidence/review-2026-10-29/local-pr36-inla/environment.txt).

## Limites e requisitos de migração

Na comparação em modo `none`, as contagens ajustadas e seus limites reproduzem
as notificadas. A data de versão foi fixada em 22/10/2026 e a semente em 42.
As duas comparações usam dados sintéticos, e o calendário de referência limita
os períodos aceitos. O benchmark de concorrência observou diferenças em
limites posteriores ao alterar o número de threads de ajuste.

Antes de migrar o consumidor, comparar a versão implantada e a candidata com
a mesma carga operacional completa, em ambiente isolado. O baseline informado
na revisão é de cerca de 2h40 com oito cores em um servidor de 12 CPUs lógicas
e aproximadamente 63 GiB de RAM. A avaliação deve registrar tempo, pico de
memória, falhas e resultados epidemiológicos, incluindo os intervalos
posteriores. As tolerâncias científicas devem ser acordadas antes da análise
e as divergências revisadas pelos responsáveis de domínio. A auditoria de
Zika em homologação também permanece como requisito da migração.

Os testes bloqueiam regressões no recorte controlado e incluem mudanças
deliberadas em contagem, ausência e identidade para provar que o comparador as
detecta. O gate de migração dos consumidores e a aprovação da release continuam
independentes destes resultados técnicos.
