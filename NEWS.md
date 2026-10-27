# AlertTools 1.2.0

## Refactoring baseline

- Added an inventory of the legacy public API and internal consumers.
- Added synthetic fixtures and characterization tests for epidemiological
  weeks, geocodes, Rt estimates, and alert classification.
- Began package metadata, dependency, documentation, and CI cleanup.
- Isolated all SQL behind an internal DBI repository with centralized physical
  table mappings, bound values, quoted identifiers, and stable domain schemas.
- Removed fallback to a global `con`; database-backed public functions now
  require an explicit `datasource` connection.
- Replaced row-at-a-time writes with transactional staging-table operations and
  explicit `error`, `update`, and `ignore` conflict policies.
- Added integration coverage for cases, individual records, climate,
  municipalities, regions, population, parameters, tweets, station links,
  rollback, idempotency, and SQL-injection attempts.
- `tabela_historico()` and `tabela_historico_intra()` now receive model
  parameters from their input attribute or an explicit `parameters` argument;
  they no longer query a database during transformation.
- Added an algorithmic Sunday-to-Saturday epidemiological calendar through
  `as_epiweek()`, `epiweek_start()`, and `epiweek_seq()`, without a fixed year
  range.
- Added the pure in-memory `alerttools_pipeline()` and
  `calculate_incidence()` computational entry points.
- Added the S3 `alerttools_result` contract and its `print()`, `summary()`, and
  `as.data.frame()` methods.
- Removed the implicit `caselist.RData` write/read path; individual records are
  now passed in memory.
- Nowcasting and generation-time computations default to one validated worker;
  stochastic nowcasting accepts an explicit scoped seed and does not change the
  caller's random state.
- Added the experimental `snake_case` workflow: `new_alert_inputs()`,
  `fetch_alert_inputs()`, `fetch_alert_parameters()`, `run_alert_pipeline()`,
  `as_alert_history()`, and `write_alert_results()`.
- Added focused readers and computational names including `fetch_cases()`,
  `fetch_case_records()`, `fetch_climate()`, `estimate_rt()`,
  `nowcast_cases()`, `define_alert_rules()`, and `classify_alerts()`.
- Legacy counterparts remain available as thin wrappers and emit staged
  deprecation warnings through `lifecycle`.
- Added `upsert_alert_parameters()` for transactional parameter batches and a
  migration table documenting functions, arguments, schemas, and intentional
  differences.
- Reworked the README with installation requirements and executable examples
  for both in-memory and database-backed workflows.
- Added reproducible in-memory and SQLite examples plus vignettes for both
  workflows and a migration guide for the experimental `snake_case` API.
- Added a pkgdown site configuration and an explicitly manual publication
  workflow guarded by a protected deployment environment.
- Documented contribution, semantic-versioning, deprecation, release-candidate,
  production-release, rollback, and proposed branch-protection procedures.
- Added focused `lintr` rules, an opt-in `styler` script, documentation-example
  tests, and a CI lint job.
- Catalogued known organizational consumers of the legacy API, including the
  national pipeline, publishers, containers, intramunicipal routines, and
  bulletins.
- Added a machine-checked migration registry, per-consumer validation and
  rollback procedure, and a CI integrity gate that deliberately blocks legacy
  API removal until critical consumers are validated and the approved support
  window has elapsed.
- Documented the proposed transition releases and historical-documentation
  retention policy; no wrapper, tag, or external release was removed or
  published.
