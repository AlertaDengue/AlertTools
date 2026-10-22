# Zika: parâmetros durante a transição

A API nova exige exatamente um parâmetro A92.8 por município. Ausências listam
os geocódigos e o CID-10; duplicidades são rejeitadas. Não há substituição por
dengue na interface nova.

O wrapper `pipe_infodengue()` mantém a política histórica de `read.parameters()`:
Zika usa parâmetros A90 e emite aviso, inclusive quando há parâmetros A92.8.
Na fronteira legada, os parâmetros são associados à doença solicitada para que
casos, resultados e histórico continuem identificados como A92.8. Parâmetros
fornecidos explicitamente pelo consumidor continuam sendo respeitados.

`tools/audit-zika-coverage.R` recebe um CSV com `municipio_geocodigo` e produz
um relatório por município: `specific`, `legacy_only`, `missing` ou `duplicate`.
Usa as configurações PG* e uma transação somente leitura. Exemplo, após instalar
a candidata e configurar o acesso ao banco de homologação:

```sh
Rscript tools/audit-zika-coverage.R municipios.csv cobertura-zika.csv
```

Os testes cobrem cobertura completa, parcial, ausente e duplicada em banco
descartável. Nenhum banco operacional foi consultado nesta entrega. O relatório
de cobertura real e a aprovação de troca da política legada continuam sendo
condições da migração do consumidor, não conclusões dos testes sintéticos.
