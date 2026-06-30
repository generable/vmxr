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

> **Status: early skeleton.** The package layout, public API surface, and design
> are in place; most verbs are stubs that raise a "not implemented" error. See
> [`docs/r-client-design.md`](docs/r-client-design.md) for the full design
> proposal, including the modeling data-access layer (nlmixr2 / Stan·Torsten)
> and the deployment plan for the RStudio/coder workspace.

Targets **API `0.2.x` / CLI `0.6.x`**.

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
nca   <- vmx_nca(dv, time_basis = "actual")
vmx_nca_result(nca)
```

## Design

The package has two layers (see the design doc):

1. **Low-level bindings** (`R/generated/`) — one function per OpenAPI operation,
   generated from a vendored `openapi.json` snapshot. Users rarely call these.
2. **Ergonomic layer** (`R/*.R`) — the curated, hand-written public API that adds
   polling, pagination, multipart upload, and tibble/S3 conversion.

The client holds **no business logic**: the API is the single source of truth.

## License

Proprietary — © Generable. See [LICENSE](LICENSE).
