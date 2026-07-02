# vmxr

<!-- badges: start -->
<!-- badges: end -->

A native **R client for the VeloMetrix API** (`vmx-api`). It wraps the
treatment &rarr; study &rarr; dataset &rarr; data-version &rarr; NCA &rarr;
modeling &rarr; simulation workflow in ergonomic, pipe-friendly verbs that
**block-and-poll** for asynchronous server jobs and return native R objects
(tibbles for collections, typed S3 objects for resources).

Our users are pharmacometricians who work in R/RStudio; `vmxr` keeps the whole
analysis next to their data instead of shuttling files and IDs through a shell.

> **Status: functional (v0.1.1), pre-CRAN.** The client covers the full analysis
> workflow end to end — treatments, studies, datasets & prep, data-versions,
> modeling-data tables, NCA, modeling (build runs, fits, estimates), simulation,
> and the study analysis log — validated against the live API on staging.
> Deferred: the nlmixr2 / Stan·Torsten data adapters (`vmx_nlmixr_data()`,
> `vmx_torsten_data()`), `vmx_dataset_download()`, and VPC-artifact tibble
> reshaping — each needs validation against real data first and currently raises
> a clear "not implemented" error. See [`docs/r-client-design.md`](docs/r-client-design.md)
> for the full design and the RStudio/coder deployment plan.

Targets **API `0.2.x` / CLI `0.6.x`**.

## What you can do

| Area | Verbs |
| --- | --- |
| Connect / identity | `vmx_client()`, `vmx_whoami()`, `vmx_health()` |
| Treatments / studies | `vmx_treatments()`, `vmx_study_create()`, … |
| Upload & prep | `vmx_upload()`, `vmx_prep_status()`, `vmx_prep_questions()`, `vmx_prep_answer()`, `vmx_wait()` |
| Data versions | `vmx_data_versions()`, `vmx_data_version_create()`, `vmx_data_version_table()`, `vmx_subjects()`/`vmx_pk()`/`vmx_pd()`, `vmx_model_data()` |
| NCA | `vmx_nca()`, `vmx_nca_result()` |
| Modeling | `vmx_model_build()`, `vmx_model_fits()`, `vmx_fit_subject_estimates()`, `vmx_fit_global_estimates()`, `vmx_fit_obs_vs_pred()` |
| Simulation | `vmx_dosing_input()`, `vmx_sim_population()`, `vmx_sim_result()` |
| Audit | `vmx_analysis_log()` |

## Installation

```r
# during development
pak::pak("generable/vmxr")
# or
remotes::install_github("generable/vmxr")
```

The intended production channel is a pre-baked install in the RStudio/coder
workspace image plus a GCS-backed CRAN-style repo — see §16 of the design doc.

## Usage

```r
library(vmxr)

# Auth via ~/.Renviron: VMX_API_BASE_URL, VMX_API_TOKEN (an Authentik PAT)
vmx_whoami()

tmt   <- vmx_treatment_create("Compound XYZ", indication = "atrial fibrillation")
study <- vmx_study_create(tmt, "Phase 1 SAD", phase = "1")
ds    <- vmx_upload(study, c("conc.csv", "dosing.csv"), mode = "initial", wait = TRUE)
dv    <- vmx_data_version(vmx_prep_status(ds)$data_version_id)
nca   <- vmx_nca(dv, time_basis = "observed")
vmx_nca_result(nca)
```

## Design

The package exposes a curated, hand-written public API in `R/*.R` that adds
polling, pagination, multipart upload, and tibble/S3 conversion. The OpenAPI
snapshot/codegen path is still a development task, not a shipped generated
binding layer.

The client holds **no business logic**: the API is the single source of truth.

## License

Proprietary — © Generable. See [LICENSE](LICENSE).
