# VeloMetrix R client — design proposal

**Status:** historical design record — package implemented in this repo
(`generable/vmxr`)
**Author:** (proposed)
**Related:** [vmx-services `clients/cli/`](https://github.com/generable/vmx-services/tree/main/clients/cli), [vmx-services PR #198](https://github.com/generable/vmx-services/pull/198)

> **Decisions taken (2026-06-30).** This doc was reviewed in vmx-services
> [PR #198](https://github.com/generable/vmx-services/pull/198). Resolved per
> jburos's review: (1) the package lives in a **standalone repo**
> (`generable/vmxr`), not the monorepo (see §2); (2) the package name is
> **`vmxr`** (closes the §15.2 open question); (3) the version anchor is **API
> `0.2.x` / CLI `0.6.x`** (§13). Sections below are preserved as written during
> review; where they still say "recommend `clients/r/`" read them as the
> rejected alternative, with §2's reviewer note as the rationale for the
> standalone choice.
>
> This document records the original proposal and is not the current API
> reference. The package function documentation and README describe shipped
> behavior; the authoritative wire semantics live in `vmx-contracts`.

## 1. Motivation

VeloMetrix exposes a REST API (`vmx-api`, FastAPI). Today the only first-class
client is the Python CLI ([`clients/cli`](../clients/cli)), a thin `typer + httpx`
wrapper. Our users are pharmacometricians who work primarily in **R/RStudio**
(NCA, popPK/PD modeling, simulation). For them the CLI has two frictions:

1. **Wrong environment.** They must drop to a shell, manage env vars, and shuttle
   files and JSON between R and the terminal.
2. **Verbose, multi-step, async workflows.** A single analysis is
   `upload → poll prep → (answer prompts) → data-version → nca/model → poll →
   fetch results`. In the shell every step is a separate command with IDs copied
   by hand.

A native R package collapses that into a few function calls that **block-and-poll
for you** and **return native R objects** (tibbles / S3), keeping the whole
workflow inside the R session next to the user's data.

### Goals

- First-class R client covering the full analysis workflow (treatments →
  studies → datasets → data-versions → NCA → modeling → simulation).
- Ergonomic, pipe-friendly verbs that hide async polling and expose
  server-owned cursor pages without unbounded implicit aggregation.
- Native return types: tibbles for collections, typed S3 objects for resources.
- One auth model, identical to the CLI (`VMX_API_TOKEN` Authentik PAT).
- **No business logic** — the API stays the single source of truth; the client
  never reinterprets server semantics.

### Non-goals

- Not a NONMEM/Monolix modeling engine — it *orchestrates* the server's jobs,
  it does not fit models locally.
- Not a 1:1 transliteration of CLI subcommands (that would just relocate the
  verbosity into R).
- Not a Shiny app (a separate product surface if ever wanted).

## 2. Where should it live?

**Decision: a standalone repo, `generable/vmxr`.** The original proposal
recommended the monorepo at `clients/r/` (the table and rationale below are kept
for the record); jburos's reviewer note further down made the case for a
standalone repo, and that is the path taken. The original recommendation read:
*"in this monorepo, at `clients/r/` — as a peer to `clients/cli/`."*

| Factor | Monorepo `clients/r/` | Separate repo |
| --- | --- | --- |
| Precedent | `clients/cli` already sets the `clients/<lang>` pattern | — |
| **Spec sync** | Codegen step can read the freshly-emitted `openapi.json` in the same CI run; API + client change in one PR | Spec must be published/versioned across repos; drift risk |
| Atomic changes | API contract change + client update reviewed together | Two PRs, ordering hazards |
| R/CRAN conventions | Package is in a subdir, not repo root (mild friction) | Repo root = package root (idiomatic) |
| `remotes::install_github()` | Needs `subdir = "clients/r"` | Clean `org/velometrix-r` |
| CI toolchain | Adds an R job to a Python repo | Isolated R CI |

The decisive factor is **spec sync**: the low-level bindings are generated from
`openapi.json`, which the API already emits as a CI artifact for FE codegen
([`services/api/src/vmx_api/openapi.py`](../services/api/src/vmx_api/openapi.py)).
Keeping the R client in-repo means that artifact never has to cross a repo
boundary, and a breaking API change can't merge without the client visibly
breaking in the same PR.

The one real cost — R users installing from a subdirectory — is fully handled by
`remotes`/`pak`:

```r
remotes::install_github("generable/vmxr")   # standalone repo; no subdir
```

**Escape hatch:** if the package is ever submitted to CRAN (which requires the
package at a repo root and a slower, decoupled release cadence), split it out with
`git subtree split`/a read-only mirror at that point. Start in-repo; promote only
if CRAN becomes a goal. Don't pay the two-repo tax before you have to.

> **Reviewer note (jburos) — reconsider the monorepo recommendation.** Two facts
> weaken the "spec sync is decisive" argument above: (1) `openapi.json` is
> *already* published as a public GCS artifact
> (`corewide-test-public-artifacts/vmx-api/openapi-latest.json`) so the
> **frontend** — a separate repo — codegens against it; the R client consuming
> that same artifact is the blessed pattern, not a boundary hazard. (2) This
> design already pins a vendored `inst/openapi/openapi.json` snapshot and picks
> the hand-written + contract-test path (§3 path b), so it does **not** rely on
> sharing a CI run with the emitter. Meanwhile the R package's *deployment*
> collaborators are **`gcloud-images`** (builds the RStudio/coder dev image) and
> **`gcloud-infra`** (deploys it, owns registries/IAM/hosts) — neither is
> vmx-services — so co-location buys only a weak contract benefit while imposing
> `subdir=` friction and an R CI leg on a Python repo. Decision rule: **prefer a
> standalone repo (e.g. `generable/velometrix-r`) unless we specifically want
> API-contract + R-client changes forced into a single reviewable PR and value
> that over R idiom + CI isolation.** See §16. *@ericnovik — your call; flagging
> for comment.*
>
> **Corollary — where this doc lives.** If the package moves to a standalone
> repo, this design doc shouldn't remain in `vmx-services/docs/`, where it would
> strand as the source of truth moves away. It should travel with the package
> (the new repo's `README`/`docs/` or a pkgdown article) or, for the
> pre-build/decision phase, live as a **Linear doc or issue** in the Generable
> project. Treat the copy here as a review artifact, not its permanent home.

### Proposed layout

```
clients/r/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── R/
│   ├── client.R          # connection object, base_url/token resolution
│   ├── http.R            # httr2 request/response plumbing, retries, paging
│   ├── auth.R            # PAT resolution, vmx_whoami()
│   ├── errors.R          # vmx_error condition classes
│   ├── treatments.R      # resource verbs (thin)
│   ├── studies.R
│   ├── datasets.R
│   ├── data_versions.R
│   ├── prep.R            # prep-status + prep-answers
│   ├── nca.R
│   ├── modeling.R        # catalog, options, build-runs, fits
│   ├── simulation.R
│   ├── workflow.R        # high-level orchestration verbs (the value layer)
│   ├── poll.R            # vmx_wait() generic + pollers
│   ├── print.R           # S3 print/format/as_tibble methods
│   └── generated/        # codegen output from openapi.json (do not hand-edit)
├── inst/
│   └── openapi/openapi.json   # vendored spec snapshot the bindings were built from
├── man/                  # roxygen2-generated
├── tests/testthat/
├── vignettes/
│   └── getting-started.Rmd
├── data-raw/             # codegen scripts
└── pkgdown/
```

## 3. Architecture: two layers

```
┌──────────────────────────────────────────────────────────┐
│  Ergonomic layer  (hand-written, stable, human-curated)   │
│  vmx_upload(), vmx_run_nca(), vmx_wait(), as_tibble()...   │
└───────────────▲──────────────────────────────────────────┘
                │ calls
┌───────────────┴──────────────────────────────────────────┐
│  Low-level bindings  (generated from openapi.json)        │
│  one fn per endpoint, typed params, raw list returns      │
└───────────────▲──────────────────────────────────────────┘
                │ httr2
┌───────────────┴──────────────────────────────────────────┐
│  vmx-api (FastAPI, REST/JSON, Authentik PAT bearer)       │
└──────────────────────────────────────────────────────────┘
```

- **Low-level (`R/generated/`)** — one function per OpenAPI operation
  (`op_datasets_upload()`, `op_treatments_list()`, …). Mechanical: build request,
  send, parse JSON to a list, surface HTTP errors. Regenerated whenever
  `openapi.json` changes. Users normally never call these directly.
- **Ergonomic (`R/*.R`)** — the curated public API. Adds polling, explicit
  cursor-page navigation, multipart upload, tibble/S3 conversion, and the
  workflow verbs. This layer is
  small, stable, and is what the docs and vignettes teach.

> **Codegen caveat (decide early).** The API emits **OpenAPI 3.1**. R generator
> tooling (`openapi-generator`, `rapiclient`) has weak 3.1 support. Two viable
> paths: (a) have the CI codegen step **downgrade the spec to 3.0** for
> generation only, or (b) **hand-write the thin HTTP layer** and use the spec
> purely as a drift check in CI (a test that asserts every operationId we wrap
> still exists, and no wrapped param disappeared). Given the endpoint count is
> modest and the workflow layer is hand-written regardless, **(b) is the
> pragmatic default** — generate types/enums only, hand-write the calls, and let
> a CI contract-test catch divergence. Revisit if the endpoint count balloons.

## 4. Auth & connection model

Identical mental model to the CLI: a **base URL** and an **Authentik PAT**.

- Default (implicit) — read from env / `.Renviron`, mirroring `VMX_API_BASE_URL`
  and `VMX_API_TOKEN`. Zero-ceremony for interactive use.
- Explicit — construct a client object and pass it as the `client` arg (defaults
  to the ambient one). Enables multiple environments / tokens in one session and
  keeps functions testable.

```r
# Implicit: reads VMX_API_BASE_URL + VMX_API_TOKEN (or set in ~/.Renviron)
vmx_treatments()                       # just works

# Explicit client, passed via the `client` arg (defaults to vmx_client())
con <- vmx_client(
  base_url = "https://vmx-api.staging.gnrbl.co",
  token    = Sys.getenv("VMX_API_TOKEN")     # never hard-code a PAT
)
vmx_treatments(client = con)

vmx_whoami(con)        # GET /me-style identity probe; confirms the PAT works
vmx_health(con)        # connectivity probe
```

`vmx_client()` resolves config with this precedence: explicit args →
`VMX_API_*` env → `~/.Renviron` → error with a clear message. Tokens are stored
in the object but **redacted in `print()`** and never logged.

## 5. Conventions

- **Naming:** `vmx_<noun>()` to list/fetch, `vmx_<noun>_<verb>()` for actions.
  Snake_case throughout. Every function takes `client = vmx_client()` as a
  trailing arg.
- **Pipe-first:** the primary resource (id or S3 object) is the **first**
  argument, so `|>` chains read naturally and IDs flow between steps.
- **IDs or objects:** every function accepts either a typed id string
  (`"ds_…"`) or the S3 object returned by a prior call. ID prefixes are
  validated client-side (matching the CLI's fail-fast behavior).
- **Return types:**
  - Collection endpoints → **tibble** for exactly one server-owned page (one
    row per item; list-columns for nested fields). `vmx_next_cursor()` and
    `vmx_has_next_page()` expose explicit traversal.
  - Single resource → typed **S3 object** (`vmx_treatment`, `vmx_dataset`,
    `vmx_data_version`, `vmx_nca_analysis`, …) with `print`/`format`/`as_tibble`
    methods.
  - Nested artifact endpoints return their parsed list shape unchanged where a
    stable tidy projection is not defined.
- **Async:** any verb that kicks off server work returns immediately with a
  handle; `vmx_wait()` blocks and polls. Convenience verbs (`vmx_*_sync()` or a
  `wait = TRUE` arg) do both.
- **Errors:** HTTP/validation failures raise a classed condition
  (`vmx_api_error`, `vmx_auth_error`, `vmx_timeout_error`) carrying status,
  server `reason`, and the structured 422 body — catchable with `tryCatch()`.

## 6. Proposed public API

### 6.1 Connection / identity

```r
vmx_client(base_url = NULL, token = NULL, ...) # -> vmx_client
vmx_whoami(client = vmx_client())              # -> list(user, email, ...)
vmx_health(client = vmx_client())              # -> TRUE / classed error
```

### 6.2 Treatments & studies

```r
vmx_treatments(status = NULL, client = vmx_client())            # -> tibble
vmx_treatment(id, client = vmx_client())                        # -> vmx_treatment
vmx_treatment_create(name, indication = NULL, description = NULL,
                     client = vmx_client())                     # -> vmx_treatment
vmx_treatment_update(id, ..., client = vmx_client())            # -> vmx_treatment

vmx_studies(treatment, status = NULL, client = vmx_client())    # -> tibble
vmx_study(id, client = vmx_client())                            # -> vmx_study
vmx_study_create(treatment, name, study_type = "clinical",
                 phase = NULL, ..., client = vmx_client())      # -> vmx_study
vmx_study_update(id, ..., client = vmx_client())                # -> vmx_study
```

### 6.3 Datasets & upload (the entry point users asked about)

```r
# Streamed multipart upload. `files` is a character vector of local paths.
# mode: "initial" (auto-formats a default DataVersion) | "incremental" | "replacement"
vmx_upload(study,
           files,
           mode      = c("initial", "incremental", "replacement"),
           treatment = NULL,          # inferred from `study` when possible
           config    = NULL,          # optional gecodata v2 project.yaml path (warm-start)
           wait      = FALSE,         # if TRUE, block until prep settles
           client    = vmx_client())  # -> vmx_dataset (status "uploaded")

vmx_datasets(study = NULL, treatment = NULL, client = vmx_client())  # -> tibble
vmx_dataset(id, client = vmx_client())                               # -> vmx_dataset
vmx_dataset_files(dataset, client = vmx_client())                    # -> tibble
vmx_dataset_tags(dataset, client = vmx_client())                     # -> tibble
vmx_dataset_cancel(dataset, client = vmx_client())                   # -> vmx_dataset
vmx_dataset_download(dataset, dest = ".", client = vmx_client())     # -> file paths
vmx_upload_ignore(upload, client = vmx_client())                     # ignore/unignore
vmx_upload_unignore(upload, client = vmx_client())
```

### 6.4 Prep status & answers (async formatting pipeline)

```r
vmx_prep_status(dataset, client = vmx_client())   # -> vmx_prep_status (state, data_version_id?)

# Generic blocking poller. `x` is anything with a pollable status:
# dataset, data-version, model-build-run, simulation-job.
vmx_wait(x,
         until    = NULL,          # target terminal state(s); sensible default per type
         timeout  = 900,           # methods use worker-aligned defaults
         interval = 5,
         progress = interactive(), # show a CLI progress bar
         client   = vmx_client())  # -> updated object, or vmx_timeout_error

# Prep can pause to ask questions. Inspect + answer:
vmx_prep_questions(dataset, client = vmx_client())             # -> tibble (when awaiting_input)
vmx_prep_answer(dataset, answers, client = vmx_client())       # answers: named list / data.frame / path
```

### 6.5 Data versions

```r
vmx_data_versions(treatment = NULL, study = NULL,
                  include_archived = FALSE,
                  eligible_for_modeling = NULL, client = vmx_client())  # -> tibble
vmx_data_version(id, client = vmx_client())                            # -> vmx_data_version
vmx_data_version_create(dataset, uploads,
                        prior_config = NULL, client = vmx_client())    # -> vmx_data_version
vmx_data_version_table(dv, client = vmx_client())                      # -> tibble (the curated data!)
vmx_data_version_export(dv, dest = NULL, client = vmx_client())        # -> path / data
vmx_data_version_archive(dv, client = vmx_client())
vmx_data_version_unarchive(dv, client = vmx_client())
```

> The DataVersion is also the entry point for **getting model-ready data into
> R** (nlmixr2, Stan/Torsten). That access layer is the single most
> adoption-critical part of the client and is specified separately in
> [§6.5a Modeling data access](#65a-modeling-data-access-nlmixr2--stantorsten).

### 6.5a Modeling data access (nlmixr2 / Stan·Torsten)

Modelers will not tolerate reshaping a monolithic export by hand. The principle:
**return analysis-ready tibbles by default, plus one-call adapters that emit the
exact structures `nlmixr2` and Torsten expect.** Tidy is the substrate; the two
ecosystem adapters are where the convenience lives.

#### Tidy accessors

```r
# One call -> available tidy tables + the metadata modelers need
md <- vmx_model_data(dv)          # $subjects $pk $dosing $pd $meta
md$subjects   # one row/subject: id, covariates (WT, AGE, SEX, CRCL, ...)
md$pk         # PK observations
md$dosing     # dosing events
md$pd         # long: id, time, dv, marker (GEN_uuid -> name), ...
md$meta       # units, lloq, time_basis, analyte/marker manifest, id map, dv hash

# Typed accessors (lazy; cached on the immutable dv id)
vmx_subjects(dv, client = vmx_client())                                  # -> tibble
vmx_pk(dv, analyte = NULL,
       format = c("tidy", "nonmem"),
       blq    = c("flag", "drop", "loq_half", "m3"),
       units  = c("as_reported", "si"),
       time_basis = c("observed", "nominal", "nominal_from_observed_dose"),
       client = vmx_client())                                            # -> tibble
vmx_pd(dv, marker = NULL, format = c("tidy", "nonmem"), client = vmx_client())  # -> tibble
```

#### Ecosystem adapters (the high-value pieces)

```r
# nlmixr2 / rxode2: NONMEM-layout data.frame, drops straight into nlmixr()
dat <- vmx_nlmixr_data(dv, analyte = "parent")
#   ID TIME DV AMT EVID CMT MDV RATE II ADDL SS  + covariates
fit <- nlmixr2(my_model, dat, est = "saem")

# Stan/Torsten: the ragged-array data list cmdstanr wants, assembled correctly
sd <- vmx_torsten_data(dv, analyte = "parent")
# list(nSubject, nt, start[i], end[i], iObs, nObs,
#      time, amt, rate, ii, addl, ss, evid, cmt,
#      cObs (or logCobs), nTheta, <covariates>)
fit <- mod$sample(data = sd)
```

The Torsten adapter is the pharmacometrics-specific moat: converting the long
event table into per-subject `start[i]`/`end[i]` index ranges plus an `iObs`
observation index is the fiddly derivation everyone re-implements by hand and
gets wrong. Build it once, correctly.

#### Cross-cutting requirements (where conveniences leak)

- **BLQ/LLOQ explicit, never silently dropped.** Carry a `blq` flag + `lloq`
  column; `blq=` selects the censoring scheme (Torsten users usually want the
  flag to build a censored likelihood, not imputation).
- **Dual subject IDs.** Keep the original identifier *and* a clean contiguous
  integer `ID` for the modeling tools, with the mapping in `$meta`.
- **Dosing fidelity from the server, not inferred:** `RATE` (zero-order/infusion
  duration), `II`/`ADDL`/`SS` (steady state), route/compartment mapping.
- **Units + time basis first-class.** Explicit units (`as_reported` vs `si`) and
  `time_basis` (`observed`/`nominal`/`nominal_from_observed_dose`) — a wrong
  assumption here silently corrupts every fit.
- **Versioned + reproducible.** Everything keys off the immutable `dv_…` id; the
  content hash is returned in `$meta` so a modeling dataset is citable/repro for
  regulatory use.
- **Caching + columnar for size.** Cache by dv id; pull large tables as
  **Arrow/Parquet** (→ tibble), not CSV.

#### API support this assumes

- **Separate typed sub-table endpoints** rather than one flat table:
  `GET /data-versions/{id}/tables/{subjects|pk|dosing|pd|labs|covariates}`.
- **A manifest** on the DataVersion: analytes, PD markers (`GEN_uuid` → human
  name), units, LLOQ, compartment map, dose routes, available time bases — the
  adapters read this to assemble correct `CMT`/`RATE`/censoring without guessing.

#### CLI / pipeline parity

Same tables for non-R users and CI:

```bash
vmx data-versions export <ds_or_dv> --table pk       --format parquet
vmx data-versions export <ds_or_dv> --table subjects --format csv
```

### 6.6 NCA

```r
vmx_nca_analyses(data_version = NULL, study = NULL, treatment = NULL,
                 status = NULL, time_basis = NULL, client = vmx_client())  # -> tibble
vmx_nca(data_version, time_basis, ..., wait = TRUE, client = vmx_client()) # create (+optionally wait)
vmx_nca_get(id, client = vmx_client())                                     # -> vmx_nca_analysis
vmx_nca_result(nca, client = vmx_client())                                 # -> tibble (PK params)
```

### 6.7 Modeling

```r
vmx_model_catalog(data_version = NULL, client = vmx_client())          # -> tibble
vmx_model_describe(model_name, client = vmx_client())                  # -> list/S3
vmx_modeling_options(data_version, time_basis, pd_marker = NULL,
                     covariate = NULL, client = vmx_client())          # preview -> list

vmx_model_build(data_version, time_basis,
                pd_marker = NULL,      # "GEN_UUID:increasing" | "...:decreasing"
                covariate = NULL,
                idempotency_key = NULL, retried_from = NULL,
                wait = FALSE, client = vmx_client())                   # -> vmx_model_build_run
vmx_model_build_runs(data_version = NULL, study = NULL, treatment = NULL,
                     status = NULL, client = vmx_client())             # -> tibble
vmx_model_build_status(run, client = vmx_client())                    # -> vmx_model_build_run
vmx_model_build_logs(run, client = vmx_client())                     # -> tibble/character
vmx_model_build_events(run, client = vmx_client())                   # -> tibble
vmx_model_build_results(run, client = vmx_client())                  # -> list/S3
vmx_model_build_artifacts(run, dest = ".", client = vmx_client())    # -> file paths
vmx_model_build_export(run, client = vmx_client())                   # -> markdown export
vmx_model_build_report(run, client = vmx_client())                   # -> signed HTML report URL
vmx_model_build_cancel(run, client = vmx_client())

vmx_model_fits(run = NULL, data_version = NULL, model_type = NULL,
               marker_name = NULL, status = NULL, client = vmx_client())  # -> tibble
vmx_model_fit(id, client = vmx_client())                                  # -> vmx_model_fit
vmx_fit_subject_estimates(fit, client = vmx_client())                     # -> tibble
vmx_fit_global_estimates(fit, client = vmx_client())                      # -> tibble
vmx_fit_obs_vs_pred(fit, client = vmx_client())                           # -> tibble (ggplot-ready)
vmx_fit_vpc(fit, client = vmx_client())                                   # -> tibble (ggplot-ready)
```

### 6.8 Simulation

```r
vmx_dosing_input(fit, dosing_text, scenario_name, client = vmx_client())  # -> vmx_dosing_input

vmx_sim_existing_subject(fit, spec, wait = FALSE, client = vmx_client())  # -> vmx_simulation_job
vmx_sim_hypothetical_subject(fit, spec, wait = FALSE, client = vmx_client())
vmx_sim_population(fit, spec, wait = FALSE, client = vmx_client())
vmx_sim_status(job, client = vmx_client())                                # -> vmx_simulation_job
vmx_sim_result(job, client = vmx_client())                                # -> tibble
vmx_sim_cancel(job, client = vmx_client())
```

### 6.9 Audit log

```r
vmx_analysis_log(study, kind = NULL, event_type = NULL, outcome = NULL,
                 severity = NULL, since = NULL, resource = NULL,
                 client = vmx_client())   # -> tibble
```

## 7. Worked examples — the verbosity payoff

**The whole CLI workflow, in-session:**

```r
library(vmxr)
# auth via ~/.Renviron: VMX_API_BASE_URL, VMX_API_TOKEN

tmt   <- vmx_treatment_create("Compound XYZ", indication = "atrial fibrillation")
study <- vmx_study_create(tmt, "Phase 1 SAD", phase = "1")

ds <- vmx_upload(study,
                 files = c("conc.csv", "dosing.csv"),
                 mode  = "initial",
                 wait  = TRUE)            # blocks through the prep pipeline

# If prep paused for input, answer and resume:
if (vmx_prep_status(ds)$state == "awaiting_input") {
  q <- vmx_prep_questions(ds)
  vmx_prep_answer(ds, answers = my_answers)
  vmx_wait(ds)
}

dv  <- vmx_data_version(vmx_prep_status(ds)$data_version_id)
nca <- vmx_nca(dv, time_basis = "observed")        # creates + waits

library(ggplot2)
vmx_nca_result(nca) |>
  ggplot(aes(time, concentration, group = subject_id)) + geom_line()
```

**Pipe-friendly, IDs flow implicitly:**

```r
vmx_treatment("tmt_01KT5QSYVYCR9HCBVCKXBVCT8X") |>
  vmx_studies() |>
  dplyr::filter(name == "Phase 1 SAD") |>
  dplyr::pull(study_id) |>
  vmx_datasets()
```

**Fit a model and pull diagnostics as tibbles:**

```r
run <- vmx_model_build(dv, time_basis = "observed",
                       pd_marker = "GEN_abc123:decreasing", wait = TRUE)
fit <- vmx_model_fits(run = run) |> dplyr::slice(1) |> vmx_model_fit()

vmx_fit_obs_vs_pred(fit) |>
  ggplot(aes(pred, obs)) + geom_point() + geom_abline()
```

## 8. Async / polling design

- `vmx_wait()` is an S3 generic dispatching on the handle type, each with a
  sensible default terminal state (dataset → `formatted`/`awaiting_input`;
  model-build-run → `succeeded`/`failed`; sim-job → `succeeded`/`failed`).
- Exponential-ish backoff capped at `interval`; honors `timeout`; emits a
  `cli`/`progress` bar in interactive sessions.
- Terminal-but-unsuccessful states raise a classed error (mirrors the CLI's
  exit-code-1 semantics) so scripts fail loudly rather than hang.
- `wait = TRUE` convenience args on `vmx_upload`/`vmx_nca`/`vmx_model_build`/
  `vmx_sim_*` are thin wrappers over `create() |> vmx_wait()`.

## 9. Errors

```r
tryCatch(
  vmx_treatments(),
  vmx_auth_error    = function(e) cli::cli_abort("Check VMX_API_TOKEN: {e$reason}"),
  vmx_api_error     = function(e) print(e$body),     # structured 422 fields
  vmx_timeout_error = function(e) message("still running; re-poll later")
)
```

Condition classes: `vmx_error` (parent) → `vmx_auth_error` (401/unauthenticated),
`vmx_api_error` (4xx/5xx with server `reason` + body), `vmx_timeout_error`
(`vmx_wait` exceeded), `vmx_usage_error` (client-side validation, e.g. bad id
prefix).

## 10. Dependencies

- **`httr2`** — requests, retries, auth, multipart, cursor pagination.
- **`jsonlite`** — JSON (de)serialization.
- **`tibble`** / **`vctrs`** — return types, list-columns.
- **`cli`** — messages, progress bars, rich errors.
- **`rlang`** — conditions, arg checking.
- Suggested (not hard deps): `dplyr`, `ggplot2` (examples/vignettes only).

Deliberately lean — no `tidyverse` hard dependency; works in a vanilla R install.

## 11. Codegen & contract-sync pipeline

1. API CI emits `openapi.json` (already done:
   [`vmx_api.openapi`](../services/api/src/vmx_api/openapi.py)).
2. A `clients/r/data-raw/codegen.R` step vendors that spec into
   `inst/openapi/openapi.json` and (path b) generates enum/type constants only.
3. Add a **contract test** (`tests/testthat/test-contract.R`) asserting every
   `operationId` the ergonomic layer relies on still exists in the spec and that
   no wrapped parameter/enum value vanished. This is the cheap insurance that the
   hand-written layer can't silently drift from the API.
4. CI job matrix adds an R leg (`R CMD check`, `testthat`, `lintr`) gated on
   `clients/r/**` changes.

## 12. Testing

- **`httptest2`** to record/replay API fixtures — no live calls in unit tests.
- A small **live smoke test** against staging, opt-in via
  `VMX_RUN_LIVE_TESTS=1` + a staging PAT, covering the §7 end-to-end path.
- Contract test (§11.3) runs on every PR touching either the API or the client.

## 13. Docs & distribution

- **roxygen2** for reference docs; **pkgdown** site.
- Vignettes: *Getting started* (auth → upload → NCA), *Modeling & simulation*,
  *Async & polling*.
- Install (pre-CRAN): `remotes::install_github("generable/vmxr")` (standalone
  repo; no `subdir`).
- Versioning tracks the API minor it targets, documented in a compatibility
  table. The current target is **API `0.2.x` / CLI `0.6.x`** (per
  `services/api/pyproject.toml` and `clients/cli/pyproject.toml`) — confirm
  against staging before building rather than assuming a `0.5` surface.

## 14. Phased roadmap

| Phase | Scope | Outcome |
| --- | --- | --- |
| **0 — Spike** | `vmx_client`, `vmx_whoami`, `vmx_treatments`, `vmx_upload`, `vmx_wait` against staging | Validate ergonomics & auth end-to-end |
| **1 — Core workflow** | treatments, studies, datasets, prep, data-versions, NCA; tibble/S3 returns; errors; `httptest2` tests | Replaces the CLI for the upload→NCA path |
| **2 — Modeling & sim** | model catalog/options/build-runs/fits, simulation jobs, artifacts/reports | Full analysis lifecycle in R |
| **3 — Polish** | pkgdown, vignettes, contract-test CI, compatibility table | Release-quality; decide on CRAN/ split-out |

## 15. Open questions

1. **Codegen depth** — generate the full HTTP layer (needs the 3.1→3.0 downgrade)
   or hand-write calls + generate only types and rely on the contract test?
   (Leaning hand-written; see §3 caveat.)
2. ~~**Package name** — `velometrix`, `vmxr`, or `vmx`?~~ **Resolved: `vmxr`.**
3. **Client object vs. fully implicit** — ship both (proposed) or force one?
4. **Result shapes** — which endpoints should return ggplot-ready long tibbles
   vs. raw nested structures? Needs a pass with a pharmacometrician.
5. **CRAN** — is public CRAN distribution a goal, or is GitHub/internal install
   sufficient? (Determines the eventual repo split.)

## 16. Deployment & install into the coder/RStudio workspace (reviewer addendum, jburos)

*Added during review of this proposal — for @ericnovik's comment. The proposal
covers the package design but not how it actually reaches users. The user-facing
environment is the **Authentik-gated RStudio "coder" workspace**, whose image is
built in **`gcloud-images`** and deployed via **`gcloud-infra`** — so the items
below land partly here and partly as sibling PRs in those repos. There is no R
tooling in `vmx-services` today; this is greenfield.*

1. **Distribution channel — hybrid, not `install_github` alone.** The §2 default
   (`remotes::install_github(..., subdir=)`) requires a `GITHUB_PAT` with read on
   a private repo plus a compile toolchain in every workspace — fragile for
   non-developer pharmacometricians. Better, mirroring existing infra:
   - **Pre-bake a pinned version into the RStudio image** (`gcloud-images`) →
     zero-friction default; it's just `library(velometrix)`.
   - **Plus a GCS-backed CRAN-style repo** (`drat`/`miniCRAN` — a static
     `PACKAGES` tree in a bucket) so users can `install.packages()` /
     `update.packages()` between image rebuilds. This is the analog of the
     `python-external` Artifact Registry repo, but GCS-static, because **AR has
     no native R repo type** and `install.packages()` can't read AR.
2. **Release workflow** — `publish-vmx-rpkg.yaml` triggered by `vmx-r-v*` tags,
   mirroring `publish-vmx-cli.yaml`: `R CMD build` → `drat::insertPackage` →
   `gcloud storage rsync` to the bucket. Registry bucket + IAM writer provisioned
   in `gcloud-infra` alongside `python-registry.tf`.
3. **System deps & install time (the real image gotcha).** `httr2`/`jsonlite`/
   `vctrs` need `libcurl`/`openssl` headers, and §6.5a's Arrow/Parquet path pulls
   in the heavy `arrow` package (`libarrow`). Building these from source in-image
   is slow/flaky — point the image at **Posit Public Package Manager (p3m.dev)
   binaries** for the base distro. If Parquet isn't day-1, gate the `arrow`
   adapters behind `Suggests` so the core package installs fast.
4. **Auth into the R session (open design question).** The package reads
   `VMX_API_TOKEN` / `VMX_API_BASE_URL`, but nothing here decides *how the
   Authentik PAT lands in the RStudio session*. The CLI solves this with
   `bootstrap.sh` (workforce-pool federation → `gcloud auth`). The R package
   needs the equivalent: either the workspace injects the token into `.Renviron`
   at spawn, or `vmx_client()` reuses the workspace's existing OIDC/ADC
   credential. Since the workspace is already Authentik-gated there may be a
   token to reuse — settle this with the `authentik-rstudio` owner before
   building, as it dictates the `client.R` / `auth.R` design.
5. **Docs hosting — reuse the CLI pattern.** The CLI publishes a mkdocs-material
   site → Authentik-gated GCS bucket at `vmx-cli-docs.<workspace>.gnrbl.co`
   (`.github/workflows/publish-cli-docs.yaml`). The R-native equivalent is
   **pkgdown → same GCS static host** at e.g. `vmx-r-docs.<workspace>.gnrbl.co`.
   Recommend doing exactly that, Authentik-gated while pre-CRAN. Caveat:
   in-session `?help` + `vignette()` ship *inside* the package and are the
   priority (they work offline in the workspace from day 1); the hosted pkgdown
   site is for browsing and belongs in the §14 Phase 3 "Polish", not the spike.
