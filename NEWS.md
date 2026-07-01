# vmxr 0.0.0.9000

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
