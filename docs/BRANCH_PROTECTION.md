# Controles administrativos recomendados

Estas configurações não foram aplicadas automaticamente. Elas exigem acesso
administrativo e decisão dos mantenedores no GitHub.

Para `main` ou `master`, recomenda-se:

- exigir pull request antes de merge;
- exigir ao menos uma revisão aprovada;
- invalidar aprovações após novos commits;
- exigir resolução de conversas;
- impedir force-push e exclusão do branch;
- exigir os checks da matriz `R-CMD-check`, integração PostgreSQL e lint;
- restringir bypass aos responsáveis por incidentes;
- proteger o ambiente `pkgdown-production` com aprovação dos mantenedores.

Antes de ativar, confirme nomes exatos dos branches e checks, responsáveis por
aprovação, estratégia para correções urgentes e impacto em automações atuais.
