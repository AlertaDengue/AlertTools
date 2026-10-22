# Comparação epidemiológica controlada

## Referências e método

A revisão `27a03f26a9bf366597ef135e7da8d2f620ea9a8e` do consumidor ainda fixa
AlertTools em `e460b3b8c15ab28e8755f482d576cf7280803b34`, conforme
`scripts/install_deps.R`. As expressões das funções de cálculo e conversão
dessa dependência foram extraídas sem alterações em `tests/reference/e460b3b8`.
O manifesto registra commit, blobs originais, funções selecionadas e hashes dos
arquivos extraídos. O calendário interno foi preservado. A consulta histórica
de parâmetros é substituída somente por uma leitura dos parâmetros controlados;
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

## Limites e gate de migração

O modo `none` não executa INLA: as contagens ajustadas e seus limites reproduzem
as notificadas. Este relatório não demonstra equivalência estocástica ou
validade clínica/operacional. A data de versão foi fixada em 22/10/2026, a
semente em 42, e as séries são sintéticas. O calendário de referência também
limita os períodos aceitos pela comparação histórica.

Continuam pendentes séries operacionais anonimizadas, execução bayesiana nas
duas versões com protocolo reprodutível, tolerâncias científicas previamente
aprovadas e revisão por responsáveis de domínio. O gerador posterior antigo
não recebia semente explícita do pipeline; não se deve alegar igualdade de
realizações apenas fixando o RNG de R. A correção de propagação de semente na
candidata não constitui validação do modelo antigo.

Os testes bloqueiam regressões no recorte controlado e incluem mudanças
deliberadas em contagem, ausência e identidade para provar que o comparador as
detecta. O gate de migração dos consumidores e a aprovação da release continuam
independentes destes resultados técnicos.
