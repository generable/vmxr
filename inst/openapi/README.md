# Vendored OpenAPI spec

`openapi.json` is a pinned snapshot of the vmx-api OpenAPI document that the
bindings were generated/validated against. The API publishes it as a public GCS
artifact (`corewide-test-public-artifacts/vmx-api/openapi-latest.json`); the
same artifact the frontend codegens from.

Refresh it with [`data-raw/codegen.R`](../../data-raw/codegen.R). The snapshot is
committed so the contract test is reproducible offline. Targets API 0.2.x.
