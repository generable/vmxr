# vmxr 0.1.1

* Send upload `config_yaml` as inline YAML text, matching the current API form
  contract.
* Add wrappers for report creation, model-fit postprocessor status,
  dosing-input status, fit simulation-job listing, and simulation `from-text`
  creation endpoints.
* Fix examples to use valid time bases and clarify that OpenAPI codegen is not
  shipped yet.

# vmxr 0.1.0

First broadly functional release: the client now covers the full analysis
workflow — treatments, studies, datasets/prep, data-versions, modeling-data
tables, NCA, modeling (build runs + fits + estimates), simulation, and the
study analysis log — with typed S3 objects, tibble collections, async polling,
and classed errors, all validated against the real API (staging). Known
deferred pieces: the nlmixr2/Torsten data adapters, `vmx_dataset_download`, and
tibble reshaping of the VPC artifact (each needs real-data validation).

* Analysis log: `vmx_analysis_log()` (`GET /studies/{id}/analysis-log`,
  paginated → tibble; `since` accepts a `POSIXct`/`Date` or ISO-8601 string;
  `resource` accepts an id or object).
* Diagnostics: `vmx_fit_obs_vs_pred()` now reshapes the PK observation block
  into a one-row-per-observation tibble (predicted-concentration bands on the
  `"extra"` attribute, PD block on `"pd"`). `vmx_fit_vpc()` still returns the
  parsed nested artifact.

## Development history (0.0.0.9000)

* Initial package skeleton: directory layout, public API surface (stubs),
  `vmx_client()` connection object with redacted-token printing, the
  `vmx_error` condition hierarchy, and the design proposal under `docs/`.
* Phase 0 (GEN-2257): real httr2 HTTP layer — bearer auth, `{error:{...}}`
  envelope mapping onto `vmx_auth_error`/`vmx_api_error`, and `next_cursor`
  pagination. Implemented `vmx_whoami()`, `vmx_health()`, the `vmx_treatments()`
  / `vmx_treatment()` / `vmx_treatment_create()` / `vmx_treatment_update()`
  verbs, `vmx_upload()` (streamed multipart), `vmx_prep_status()`, and
  `vmx_wait()` (dataset/prep-status poller). Typed S3 resource objects with
  `print`/`as_tibble`, id-or-object resolution with prefix validation, and
  collection→tibble conversion. httr2-mocked unit tests plus an opt-in live
  smoke test (`VMX_RUN_LIVE_TESTS=1`).
* Studies + data-versions slice: `vmx_studies()` / `vmx_study()` /
  `vmx_study_create()` / `vmx_study_update()`; `vmx_data_versions()` /
  `vmx_data_version()` / `vmx_data_version_create()` (format job over an upload
  composition, returns a prep-status) / `vmx_data_version_archive()` /
  `vmx_data_version_unarchive()` / `vmx_data_version_export()` (signed-URL
  envelope, optional streamed download). Adds a `vmx_patch()` HTTP helper and
  `vmx_opt_id()` (NULL-passthrough id resolution). The `vmx_data_version_table()`
  and modeling-data adapters (§6.5a) remain stubbed for a dedicated slice.
* NCA slice: `vmx_nca_analyses()` (data-version/study/treatment/status/time-basis
  filters), `vmx_nca()` (create + optionally block via `vmx_wait()`),
  `vmx_nca_get()`, and `vmx_nca_result()` (reshapes `point_estimates` into a tidy
  one-row-per-subject tibble, quantity metadata on the `"quantities"` attribute).
  `vmx_wait()` gains an NCA method (terminal `completed`/`degraded` succeed,
  `failed` raises). The status poller is refactored into a generic
  `vmx_poll_status()` shared by the dataset and NCA waiters.
* Modeling slice: `vmx_model_catalog()` (categories flattened to a tibble),
  `vmx_model_describe()`, `vmx_modeling_options()`; `vmx_model_build()` (parses
  the `"GEN_uuid:increasing"` pd-marker shorthand, create + optional
  `vmx_wait()`), `vmx_model_build_runs()` / `_status()` / `_results()` /
  `_logs()` / `_export()` / `_report()` / `_cancel()`; and fits:
  `vmx_model_fits()`, `vmx_model_fit()` (details), `vmx_fit_subject_estimates()`
  and `vmx_fit_global_estimates()` (reshaped to tidy tibbles with point
  estimate + credible interval). `vmx_wait()` gains a model-build-run method
  (terminal `succeeded`/`degraded` succeed; `failed`/`cancelled` raise). The
  `vmx_nca()` / `vmx_model_build()` `wait = TRUE` paths now forward polling
  controls (`timeout`/`interval`/`progress`) to `vmx_wait()`.
  `vmx_fit_obs_vs_pred()` and `vmx_fit_vpc()` return the parsed diagnostic
  artifacts as-is for now; reshaping their nested plot bands into tibbles is a
  follow-up.
* Simulation slice: `vmx_dosing_input()`; `vmx_sim_existing_subject()`,
  `vmx_sim_hypothetical_subject()`, `vmx_sim_population()` (accept a data.frame
  of subjects/covariates or explicit records; create + optional `vmx_wait()`);
  `vmx_sim_status()`, `vmx_sim_result()`, `vmx_sim_cancel()`. `vmx_wait()` gains
  a simulation-job method (terminal `succeeded`; `failed`/`cancelled` raise).
  `vmx_sim_result()` returns the parsed result payload (reshaping deferred).
* Dataset + prep verbs: `vmx_dataset_files()`, `vmx_dataset_tags()` (key/value
  tibble), `vmx_dataset_cancel()`, `vmx_upload_ignore()` / `vmx_upload_unignore()`
  (now take `dataset` + `upload`), `vmx_prep_questions()` (prompt fields →
  tibble), and `vmx_prep_answer()` (posts a named-list answers body).
  `vmx_dataset_download()` stays unimplemented — the files listing exposes no
  per-file download URL (use `vmx_data_version_export()`).
* Modeling-data access: `vmx_data_version_table()` (any of the `subjects`/`pk`/
  `dosing`/`pd`/`labs`/`covariates` domains → a typed tibble, column metadata on
  the `"columns"` attribute), the `vmx_subjects()` / `vmx_pk()` / `vmx_pd()`
  accessors, and `vmx_model_data()` (bundles the available tables + DV metadata
  as a `vmx_model_data` object, fetching only `table_availability` domains). The
  nlmixr2 (`vmx_nlmixr_data()`) and Stan/Torsten (`vmx_torsten_data()`) adapters
  remain deferred stubs — the NONMEM/ragged-array assembly must be validated
  against real data + the DV column manifest before shipping.
