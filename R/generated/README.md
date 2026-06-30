# Generated bindings (do not hand-edit)

Files in this directory are produced by [`data-raw/codegen.R`](../../data-raw/codegen.R)
from the vendored OpenAPI spec at
[`inst/openapi/openapi.json`](../../inst/openapi/openapi.json).

Per the design (section 3, path b), codegen emits **enum/type constants only**;
the HTTP calls are hand-written in `R/*.R` and a contract test
(`tests/testthat/test-contract.R`) asserts the hand-written layer has not
drifted from the spec. Do not edit generated files by hand — re-run codegen
instead.
