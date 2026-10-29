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

## Pré-requisitos da auditoria em homologação

Na inspeção do repositório em 29/10/2026, não foram localizados configuração de
acesso à homologação nem CSV da população operacional de municípios. Os códigos
das fixtures não podem substituir esse recorte. As configurações PG* da CI
apontam para um banco descartável e não constituem acesso à homologação.

Para concluir a auditoria, o responsável pelo ambiente precisa disponibilizar o
mecanismo de acesso somente leitura e a lista de municípios. O CSV deve conter
`municipio_geocodigo` com códigos numéricos únicos de sete dígitos. O script
rejeita entradas incompletas/duplicadas, carrega a candidata quando executado na
raiz do projeto e recusa sobrescrever um relatório existente. A saída inclui
metadados de data UTC, versão, hash da entrada e número de municípios; não inclui
credenciais. Não registrar o consumidor como migrado enquanto essa evidência faltar.
