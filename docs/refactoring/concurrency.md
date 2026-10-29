# Concorrência independente

`run_alert_pipeline()`, `alerttools_pipeline()` e o wrapper legado aceitam
`municipality_workers` e `inla_threads`. Ambos começam em 1. O alias `workers`
nessas interfaces controla somente os processos por município/doença. Valores
conflitantes fornecidos pelos dois nomes são rejeitados. A ordem dos argumentos
anteriores foi preservada.

Nos leitores de uma única série `nowcast_cases()` e `bayesnowcasting()`,
`workers` permanece alias para threads do INLA. O ajuste recebe `N:1`, sem
alterar opções globais; a amostragem posterior não cria processos adicionais.
Com uma semente fixa, a amostragem usa `1:1` e passa uma semente não nula ao
INLA, conforme o contrato de reprodutibilidade do próprio motor. Semente zero
é convertida para 1 somente no motor posterior; o gerador R mantém a semente
solicitada. Windows executa o nível municipal serialmente e registra esse
limite nos metadados.

O orçamento configurável é aproximadamente `estados concorrentes *
municipality_workers * inla_threads`. O consumidor deve definir seus próprios
limites externos. A biblioteca não detecta a topologia de um consumidor nem
aumenta implicitamente um controle com base no outro.

## Benchmark reproduzível

```sh
Rscript tools/benchmark-concurrency.R benchmark-concurrency.csv
```

O script usa 200 municípios sintéticos, 3 repetições e as combinações 1/2
processos x 1/2 threads em processos R isolados. Requer Python 3 e `ps` em
macOS/Linux. Registra tempo, tamanho das
entradas, plataforma, semente e pico observado de RSS agregado do processo e
filhos, amostrado a cada 50 ms. O RSS pode contar páginas compartilhadas mais de
uma vez e inclui o carregamento das dependências; não representa memória
privada nem um pico exato. O tempo mede o cálculo, incluindo criação dos
processos municipais.

Este benchmark mede o modo `none` e não justifica aumentar os defaults nem
concluir ganhos do INLA: nesse modo o controle de threads não faz trabalho.
Uma medição bayesiana com séries operacionais anonimizadas e diferentes níveis
de concorrência externa continua necessária antes de escolher limites de
implantação. Os testes verificam equivalência serial/paralela e a propagação
separada dos limites até o ajuste e a amostragem do INLA.

## Medição controlada em 22/10/2026

CSV: `benchmarks/concurrency-2026-10-22.csv`; R 4.4.3, macOS arm64, 1.600 linhas
de casos. As medianas das três repetições foram:

| Processos | Threads | Tempo (s) | RSS agregado observado (MiB) |
|---|---|---|---|
| 1 | 1 | 0,497 | 257,8 |
| 1 | 2 | 0,495 | 255,8 |
| 2 | 1 | 0,315 | 571,8 |
| 2 | 2 | 0,315 | 573,0 |

O ganho nesse recorte veio com maior RSS observado. O tamanho e a duração são
insuficientes para definir limites operacionais. Os defaults permanecem 1/1.
O CSV registra o SHA-256 dos fontes R usados; a carga sintética e o supervisor
estão versionados com esta entrega.

## Validação com INLA real após o review de 29/10/2026

O runner dedicado executa quatro cenários obrigatórios:

```sh
Rscript tools/run-inla-integration.R /tmp/inla-evidence
```

Ele falha se INLA estiver ausente, se algum cenário for ignorado ou se a chamada
bayesiana apenas devolver os casos brutos. Os cenários cobrem repetição,
concorrência municipal com uma e duas threads de ajuste, referência histórica,
fallback real do consumidor com falhas injetadas e encerramento de um worker.
Falha de ajuste preserva seu diagnóstico; a camada paralela rejeita saída parcial.
O caso de encerramento mata um worker controlado, não força OOM no computador.

As entradas são sintéticas: 60 semanas, atrasos de zero a quatro semanas,
dois municípios e dengue/chikungunya. O comparador exige tolerância absoluta
`1e-7`, igualdade de categorias e de ausência de dados. A referência mantém os
corpos históricos imutáveis e adapta somente as dependências externas para o
mesmo ambiente e a semente explícita de amostragem. A comparação com a referência
usa quatro threads de ajuste, como o código antigo. Não valida a instalação
integral da imagem operacional antiga.

Alterar as threads do ajuste não é pressuposto numericamente idêntico. O
benchmark conserva baselines por orçamento de threads, compara concorrência e
repetições contra esses baselines e exporta separadamente diferenças entre
orçamentos. Mudanças de nível de alerta entre orçamentos falham a execução;
outras diferenças permanecem visíveis para avaliação científica.

```sh
python3 tools/benchmark-inla.py /tmp/inla-host --environment host --max-rss-mib 3072
docker build -f tools/inla-validation.Dockerfile -t alerttools-inla-validation .
```

A matriz mede uma ou duas cópias externas da carga, um ou dois processos
municipais e uma ou duas threads de ajuste, com três repetições. As cópias
representam a disputa de recursos entre estados; não são uma execução nacional.
Cada rodada registra tempo de parede incluindo startup, tempo de cálculo nos
workers, versões, entrada, semente e RSS agregado amostrado a cada 100 ms.
O supervisor encerra os grupos de processos ao exceder tempo/memória ou falhar.
O RSS pode contar páginas compartilhadas várias vezes. Em contêiner, as métricas
de cgroup têm escopo da vida inteira do contêiner e são identificadas dessa forma.

A imagem de teste fixa a base por digest e o INLA R/motor em 25.12.16, build
oficial disponível no repositório testing. Isso identifica um ambiente de teste,
não uma recomendação de versão para produção. Os builds ARM 26.08.07/26.08.22
encontrados apresentaram incompatibilidade de biblioteca/instrução na máquina
local. O instalador do build anterior requer carregar INLA; o launcher ARM
precisa de um alias do nome `inla.mkl` para o executável oficial `inla`.
Nenhum cálculo do motor é modificado. O build testa um ajuste real antes de aceitar a imagem.

Os defaults continuam 1/1. Uma medição sintética e uma imagem de teste não
substituem dimensionamento com a carga operacional e revisão científica.
