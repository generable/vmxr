# arv-022-dv fixture

A recorded **API 0.2.2 (as deployed on the arv/csl workspaces)** DataVersion, the
sibling of `staging-dv/` (which records the 0.3 shape). Used by the 0.2.2 replay
test in `test-nlmixr.R` so the 0.2.2 table/DV path has recorded-shape coverage,
not just hand-built mocks.

- Source: the **arv** workspace per-basis export, API contract **0.2.2** after the
  `9c0ac49` server hotfix (`vmx-api`), `GET /data-versions/{dv}` and
  `GET /data-versions/{dv}/tables/{domain}?time_basis=observed` for
  subjects / pk / dosing / pd / covariates. `dv.json` is trimmed to the fields
  the client reads.
- The recorded **0.2.2 shape**, distinct from 0.3:
  - `dv.json` advertises a **boolean** `time_bases` map (`{observed: true,
    nominal: false}`) and an **object** `recommended_time_basis`
    (`{value, reason}`) — not the per-basis descriptor objects of 0.3.
  - The table payloads are the **per-basis export**: the requested basis **is**
    echoed (top-level `"time_basis": "observed"`), carry a **single**
    `eligible_for_modeling` flag (not the 0.3 before/after-QC pair), use the
    basis-named `observed_time_hours` column, and omit `time_hours`.
  - The **pre-hotfix** `f8210578` canonical variant (basis **not** echoed,
    `"basis_echoed" = FALSE`) is exercised by the hand-built mock test
    "an API 0.2.2 server that ignores time_basis …" in `test-nlmixr.R`.
- Content: **synthetic study data, no PHI.** Three subjects (one oral 100 mg
  regimen, one 50 mg iv-infusion, one placebo), PK concentration (mg/L) with
  BLQ/ALQ conventions, two PD markers, weight / sex covariates. Mirrors the
  in-code builder subjects so the replay assembles the same eligible rows as the
  canonical path.
- Identifiers: every `dv_` id is an obviously fake constant and every
  `gen_*_uuid` a deterministic fake token; they do not resolve anywhere.
- Regenerate: point `vmx_client()` at an arv 0.2.2 workspace, fetch the same six
  payloads for a ready DataVersion, trim `dv.json`, and re-run the id/uuid
  rewrite.
