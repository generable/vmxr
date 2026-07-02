# Generated constants

This directory is reserved for generated constants from
[`data-raw/codegen.R`](../../data-raw/codegen.R) once the vendored OpenAPI
snapshot and contract test are implemented.

Per the design (section 3, path b), codegen will emit **enum/type constants
only**; the HTTP calls are hand-written in `R/*.R`.
