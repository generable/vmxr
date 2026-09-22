# test-03-dv fixture

A recorded API 0.3 DataVersion used by the opt-in rxode2 / nlmixr2 end-to-end
tests in `test-nlmixr.R`.

- Records: API contract **0.3** (vmx-api main `83486f9`), recorded 2026-09-02,
  `GET /data-versions/{dv}` and
  `GET /data-versions/{dv}/tables/{domain}?time_basis=observed` for
  subjects / pk / dosing / pd / covariates. `dv.json` is trimmed to the fields
  the client reads.
- Content: **synthetic study data, no PHI.** Twelve subjects, one oral 1.5 mg/kg
  regimen, PK concentration (mg/L) with one BLQ convention, two PD markers,
  weight / age / sex covariates. Every value is fabricated.
- Identifiers: every resource id (`dv_`, `std_`, `tmt_`) is replaced by an
  obviously fake constant and every `gen_*_uuid` by a deterministic fake UUID;
  they do not resolve anywhere.
- Regenerate: point `vmx_client()` at any workspace serving that contract
  version, fetch the same six payloads for a ready DataVersion, trim `dv.json`,
  and re-run the id/uuid rewrite (see the history of this directory for the
  script).
