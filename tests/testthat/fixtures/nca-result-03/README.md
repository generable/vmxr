# nca-result-03 fixture

A recorded **API 0.3** NCA result (`GET /nca-analyses/{id}/result`), replayed
byte-for-byte by the "recorded 0.3 result" test in `test-nca.R` so the
cursor-paged `items[]` reader is checked against a shape the server actually
served, not only against hand-built mocks.

- Source: the VeloMetrix **arv-staging** workspace, 2026-09-14, API contract 0.3
  (vmx-api `5083fac`, NCA worker `nca/0.13.4`), the automatic NCA the
  event-router ran on a freshly formatted DataVersion (`inputs`:
  `time_basis = observed`, `bloq_handling = discard`). The body is kept whole:
  every envelope key and every item sub-structure is as served, including the
  `null` entries of `resolved_time_intervals_hours`, the per-subject
  `not_estimable_reasons` maps, the `quantities` descriptors and the `units`
  map.
- Content: **synthetic study data, no PHI.** The 32-subject single-dose
  warfarin teaching dataset (the same study `staging-dv/` records a
  DataVersion of): one item ("First dosing interval"), 19 PK quantities. On a
  single-dose study the intended tau is unavailable, so `c_avg`, `auc_tau`,
  `auc_interval` and their duration companions are `null` for every subject
  with a reason (`intended_tau_not_available_for_subject`,
  `open_ended_dosing_interval`); `cmax`, `auc_inf`, `t_half`, `cl`, `v` and the
  rest are populated for all 32.
- Identifiers: `nca_id`, `data_version_id` and `current_data_version_id` are
  obviously fake constants and every `gen_subject_uuid` is a deterministic fake
  UUID (`0000000N-0000-4000-8000-00000000000N`); they do not resolve anywhere.
  `subject_id` values are the study's own integer labels.
- Regenerate: from a workspace on a v0.3 environment,
  `vmx --format json nca-analyses result <nca_id> > result.json` for a completed
  NCA on a synthetic study, then rewrite the three resource ids and the
  `gen_subject_uuid` arrays (excluded-subject entries included) with the
  deterministic map above. No trimming is applied.
