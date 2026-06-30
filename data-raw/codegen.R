# codegen.R — vendor the OpenAPI spec and (re)generate type/enum constants.
#
# Path (b) from the design (section 3): we hand-write the HTTP layer and use the
# spec purely as a drift check, generating only enum/type constants. Run this
# script when the API contract changes:
#
#   Rscript data-raw/codegen.R
#
# Steps:
#   1. Download the published spec to inst/openapi/openapi.json.
#   2. Emit enum/type constants into R/generated/.
#   3. Leave the hand-written calls in R/*.R untouched; the contract test
#      (tests/testthat/test-contract.R) verifies they still match the spec.

# Published artifact (see inst/openapi/README.md).
OPENAPI_URL <- Sys.getenv(
  "VMX_OPENAPI_URL",
  "https://storage.googleapis.com/corewide-test-public-artifacts/vmx-api/openapi-latest.json"
)
SPEC_PATH <- "inst/openapi/openapi.json"

stop("codegen.R is a skeleton stub; implement spec download + constant emission.")
