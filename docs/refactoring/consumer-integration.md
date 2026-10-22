# Recorte integrado do consumidor

O teste executa expressões sem alterações de `main/main_BR.R` na revisão
`27a03f26a9bf366597ef135e7da8d2f620ea9a8e` do AlertaDengueAnalise. O pin do
AlertTools continua sendo `e460b3b8c15ab28e8755f482d576cf7280803b34`. O manifesto
em `tests/reference/consumer-27a03f26` registra o blob original e os hashes das
funções e dos três blocos reais de exportação SQL selecionados.

`tools/prepare-consumer-reference.R` extrai as expressões do fonte original,
verifica seu blob e confirma igualdade das expressões R. Não executa o startup
do consumidor, sua configuração global, SSH/SCP ou um banco operacional.

## O que é executado

- `run_disease_pipeline()`, `run_disease_pipeline_by_city()`,
  `run_city_pipeline()` e `combine_ale_parts()` reais;
- processamento estadual de dois municípios, falha estadual injetada com
  recuperação completa, isolamento de um município com falha e falha de todos;
- dengue, chikungunya e Zika com fallback legado;
- `load_state_result()` e `normalize_ale()` reais para verificar o objeto `res`
  em `.RData`, sua estrutura e conteúdo, sem exigir identidade binária;
- os três blocos reais de exportação SQL por doença;
- persistência DBI idempotente; no PostgreSQL, replay dos SQLs produzidos pelo
  escritor da referência e pelo escritor candidato, comparando todas as colunas.

As entradas são sintéticas, usam as consultas reais da candidata e um banco
descartável. As funções de cálculo, conversão e SQL da referência são as do pin
antigo; sua leitura é adaptada à mesma entrada DBI controlada. Esse adaptador não
valida o SQL de leitura antigo ou a instalação integral da imagem operacional.
O modo é `none`, sem inferência de equivalência bayesiana. A precisão numérica é
comparada com tolerância absoluta `1e-7`; há um teste específico de exportação
com `1e-12` para detectar coerção/formatação acidental de probabilidades.
As tabelas são alinhadas pelas chaves municipais, doença, semana e localidade
antes da comparação: a API nova ordena municípios, enquanto o consumidor antigo
pode preservar a ordem de consulta. Chaves duplicadas ou incompletas falham.

## Correções de compatibilidade encontradas

A recuperação municipal concatena listas com `c()`, perdendo o atributo externo
de parâmetros. Cada objeto `alerta` agora carrega os próprios parâmetros como
atributo, sem alterar seus campos públicos. O conversor recupera esses valores
dos filhos e continua rejeitando dados incompletos ou conflitantes.

A geração SQL preserva os tipos de cada célula ao cotar literais, em vez de
converter uma linha inteira em matriz de caracteres. Isso evita formatação
numérica dependente da coerção. A persistência continua com conexão explícita;
nenhum efeito de escrita foi introduzido no cálculo.

## CI e evidências

O job existente de PostgreSQL usa PostgreSQL 16 efêmero e executa
`tools/run-postgres-integration.R`. O runner exige `ALERTTOOLS_TEST_POSTGRES=true`,
o banco dedicado `alerttools_test` e RPostgres instalado; falha se o cenário
PostgreSQL do consumidor for ignorado. Upload preserva CSVs de comparação,
RData/SQLs de ambas as versões, resultados da suíte, ambiente e manifests,
inclusive em uma falha de verificação.

```sh
ALERTTOOLS_TEST_POSTGRES=true PGDATABASE=alerttools_test \
  Rscript tools/run-postgres-integration.R consumer-evidence
```

O helper de integração recria schemas somente no banco dedicado de teste.
Para uma validação local sem PostgreSQL, o teste SQLite exercita os mesmos
contratos, artefatos e persistência, mas não substitui o replay PostgreSQL da CI.

## Limites de aprovação

Isto é um recorte representativo do consumidor com origem verificável, não uma
execução nacional do `main_BR.R`. Permanecem pendentes infraestrutura e dados
operacionais anonimizados, avaliação bayesiana, publicação/implantação externa,
responsáveis e aprovação científica. O registro de consumidores não é marcado
como migrado ou validado em produção por causa destes testes. A troca da
dependência no repositório consumidor continua sendo um PR separado, após
essas evidências e seu plano de rollback.
