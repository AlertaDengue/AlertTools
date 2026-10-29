# Correções dos reviews: implementação e validação local

Data: 29/10/2026. Candidata consolidada: `codex/review-corrections-2026-10-29`.

As correções de código foram implementadas, testadas e organizadas em nove
branches locais `codex/review-ready/*`. Nenhuma branch foi enviada ao GitHub,
nenhum PR foi mesclado e nenhuma release ou migração foi executada.
O estado público dos PRs ainda não contém estas alterações.

## Atendimento dos comentários

| Destino | Correção local | Evidência principal |
|---|---|---|
| #28 | Preservar parâmetros em cada alerta municipal e recuperá-los após `do.call(c, ...)`; rejeitar parâmetros ausentes/conflitantes sem consultar o banco | `test-legacy-fallback.R`; helper validado antes do join, sem depender do #32/#37 |
| #30 | Isolar a leitura da versão; testar 1.2.0/1.2.9 sem aviso e 1.3.0/1.4.0 com aviso real de `lifecycle`; documentar a política aprovada | `test-deprecation-policy.R`, `api-migration.md` |
| #31 | Antecipar do #32 o processamento completo por município/doença, inclusive histórico e validações dos pares; apontar pares ausentes/excedentes nos erros | `test-pipeline-group-keys.R`, `test-core-pipeline.R` |
| #31 | Corrigir regressão encontrada na validação: entradas em ordem inversa alteravam Rt e alertas; ordenar semanas antes do cálculo | `test-pipeline-group-order.R`: execução conjunta/reordenada versus execuções isoladas |
| #32 | Rejeitar múltiplos CID10 ou divergência entre dados e parâmetros antes da conversão para objetos legados | `test-legacy-disease-guard.R`; API moderna continua aceitando múltiplas doenças |
| #33 | Validar CSV e preservar procedência da auditoria somente leitura, sem sobrescrever relatórios | CLI exercitada no PostgreSQL descartável; homologação continua pendente |
| #34 | INLA real serial/paralelo, repetição e worker encerrado; preservar diagnóstico do ajuste e impedir retorno parcial como completo | Dois cenários próprios obrigatórios, seis verificações no snapshot independente do #34 |
| #34 | Benchmark com concorrência externa/municipal/threads, tempo e memória no host e em Docker | 48 rodadas; relatório em `benchmarks/inla-2026-10-29.md` |
| #35 | Preservar o contrato aprovado de `minimum_history`/`dy`, sem habilitar regra epidemiológica | ASTs dos 15 fontes R idênticas às do #34; alteração documental |
| #36 | Acrescentar comparação bayesiana real com os corpos históricos imutáveis e o orçamento antigo de quatro threads | Cenário próprio de referência; runner exige três cenários nessa etapa |
| #37 | Acrescentar fallback bayesiano real do consumidor, isolamento de falha e artefatos; preservar precisão SQL e replay PostgreSQL existentes | Runner final exige quatro cenários; referências e recuperação não são dependências antecipadas do #34 |

## Validação concluída

| Verificação | Resultado |
|---|---|
| Suíte completa com PostgreSQL 16 descartável | 474 verificações aprovadas, zero falhas/warnings; quatro cenários INLA reservados ao runner dedicado |
| INLA real no macOS ARM | Quatro cenários obrigatórios, 14 verificações, zero falhas/warnings/skips |
| INLA real no Linux ARM/Docker | Quatro cenários obrigatórios, 14 verificações, zero falhas/warnings/skips |
| #34 isolado dos PRs posteriores | Dois cenários obrigatórios de INLA real e seis verificações aprovadas |
| #36 isolado do consumidor do #37 | Três cenários obrigatórios de INLA real e sete verificações aprovadas |
| Pontos intermediários #28/#30/#31/#32/#33/#34/#36 | Testes pertinentes aprovados, com warnings tratados como falha no verificador local |
| Lint | Aprovado em Docker |
| R CMD check em Docker | Zero erros, zero warnings, uma nota sobre o `con` global legado |
| Auditoria CLI | Somente leitura, metadados, bloqueio de sobrescrita e de municípios duplicados aprovados no banco descartável |

O check construiu as vignettes, usou `--no-manual`, desativou a exigência de
todos os Suggests e a verificação do relógio. A nota de `con` corresponde ao
default legado preservado por solicitação de compatibilidade; não foi ocultada.
Os testes bayesianos obrigatórios foram executados separadamente, sem skips.
Mensagens nativas de NUMA/`mbind` do contêiner permanecem nos logs; nenhuma
permissão privilegiada foi acrescentada para suprimi-las.

As evidências estão em [evidence/review-2026-10-29](evidence/review-2026-10-29).
Os CSVs registram testes, fontes, ambientes, repetições e comparações. RDS/RData/SQL
contêm apenas a carga sintética utilizada nesta validação.

## Resultado do benchmark

Houve 2.088 comparações coincidentes por ambiente, mantendo fixo o orçamento
de threads. Ao mudar esse orçamento, diferenças nos limites dos intervalos
posteriores continuaram marcadas como `mismatch`: até uma unidade no inferior
e até 0,025 no superior. Casos medianos, Rt e alertas coincidiram nesse recorte.
As tolerâncias não foram ampliadas para aprovar esses limites diferentes.

No Docker de duas CPUs, acrescentar paralelismo municipal a duas cópias externas
aumentou tempo e RSS. Os defaults permanecem 1/1. RSS agregado pode contar
memória compartilhada várias vezes; a interrupção pelo orçamento de 2 GiB não
foi OOM do computador. A matriz completa foi concluída com orçamento explícito
de 3 GiB. Consultar [o relatório de recursos](benchmarks/inla-2026-10-29.md).

Os ambientes têm versões distintas: macOS R 4.4.3/INLA 24.12.11 e Linux
R 4.6.1/INLA 25.12.16. O build oficial 25.12.16 do repositório testing foi
fixado para o ambiente ARM de teste; isso não recomenda uma versão para produção.
A comparação histórica usa o mesmo motor nos dois lados e explicita a adaptação
de sementes/dependências externas. Não é uma execução da imagem nacional antiga.

## Cadeia local e preservação do histórico

```text
master ← #20 ← #28 ← #30 ← #31 ← #32 ← #33 ← #34 ← #35 ← #36 ← #37
```

- #27 e #29 continuam mesclados e seus commits de merge foram preservados como
  ancestrais do #28 preparado. A base atual foi incorporada sem perder o #29.
- O problema de parâmetros já corrigido no #37 foi antecipado para o #28.
- O processamento por município/doença do antigo #32 foi antecipado para o #31;
  o novo diff do #32 se concentra na proteção do adaptador legado.
- O comparador comum e o teste de concorrência chegam no #34. A referência
  histórica chega no #36 e o cenário do consumidor no #37. O runner exige
  respectivamente dois, três e quatro cenários, sem aprovação por ausência de testes.
- A implementação duplicada do helper de parâmetros foi removida na reaplicação
  do #37. Fontes, testes e workflows da ponta local correspondem à candidata consolidada.
- Foram criadas referências de segurança antes da reconstrução. Nenhuma ref
  remota mudou durante o trabalho. Os PRs #27/#29 não foram reabertos.

O [manifesto das branches](evidence/review-2026-10-29/ready-refs.json) registra
as refs de preparação e as heads remotas observadas. SHAs de validação podem
anteceder commits somente de evidências/documentação. Antes de publicar,
resolver novamente a ponta de cada branch e conferir o remoto.

## Pendências e próxima etapa

1. **Auditoria Zika em homologação:** o repositório não fornece configuração de
   acesso nem a lista operacional de municípios. Não substituir por fixtures,
   credenciais da CI ou municípios deduzidos. A ferramenta está pronta e testada;
   a execução real depende desses dois insumos.
2. **Avaliação científica/operacional:** avaliar os intervalos posteriores ao
   mudar threads e dimensionar a carga nacional antes de migrar o consumidor.
   Não alterar seu registro para migrado ou validado em produção.
3. **Publicação, conforme a próxima instrução do usuário:** atualizar refs,
   reconciliar qualquer alteração externa e enviar as branches preparadas para
   as branches originais dos PRs, de pais para filhos, com lease no SHA observado.
   Não fazer push irrestrito nem merge automático.
4. **CI/revisão depois do envio:** conferir os novos checks no SHA publicado e
   solicitar nova revisão quando autorizado. Os checks antigos e aprovações
   anteriores não certificam as novas heads.

## Respostas preparadas para os reviews

Textos para usar depois da publicação; não foram postados.

**#28:** A correção foi antecipada do #37. Cada alerta municipal preserva seus
parâmetros e o histórico os recupera após a concatenação real do fallback, sem
consulta implícita ao banco. O teste cobre parâmetros diferentes por município,
ausência e conflito. O merge do #29 foi preservado na atualização da base.

**#30:** A leitura da versão foi isolada e os testes agora exercitam ausência de
aviso em 1.2.x e emissão real de `lifecycle` desde 1.3.0. O guia reflete a política
aprovada. A branch foi reaplicada sobre a base que já contém o #29.

**#31:** O cálculo, as validações e o histórico usam município/doença de forma
coerente. A implementação que estava no #32 foi antecipada para este PR. Os
erros identificam pares ausentes/excedentes; testes verificam doenças distintas
e entradas reordenadas. Foi corrigida também a ordenação semanal antes de Rt/alertas.

**#32:** O conversor legado rejeita explicitamente múltiplos CID10 e inconsistência
entre dados e parâmetros. A API moderna preserva o processamento de várias
doenças. O diff foi reduzido ao escopo restante após a antecipação do cálculo para o #31.

**#33:** A auditoria ganhou validação de entrada e evidências de procedência,
com teste somente leitura em PostgreSQL descartável. A política legada foi
preservada. A auditoria de homologação ainda depende de acesso e da lista
operacional de municípios, que não foram encontrados no repositório.

**#34:** Foram acrescentados testes com INLA real e um job obrigatório, sem skips.
Há medições de concorrência externa/municipal/threads no host e em Docker,
incluindo tempo, memória, repetição e interrupção controlada. Os resultados por
orçamento, diferenças entre threads e a interrupção por orçamento de RSS estão
preservados no relatório; os defaults continuam conservadores.

**#35–37:** Os contratos aprovados foram preservados na cadeia local. O #36
acrescenta a comparação bayesiana real e o #37 a recuperação bayesiana do
consumidor. Precisão SQL, comparação não bayesiana, snapshots imutáveis e replay
PostgreSQL continuam cobertos. A aprovação de migração operacional permanece
condicionada às evidências de homologação e avaliação científica.
