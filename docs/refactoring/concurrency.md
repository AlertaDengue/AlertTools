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
