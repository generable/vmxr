# Vendored OpenAPI spec

`openapi.json` is intended to be a pinned snapshot of the vmx-api OpenAPI
document that the wrappers are validated against. The API publishes it as a
public GCS artifact (`corewide-test-public-artifacts/vmx-api/openapi-latest.json`);
the same artifact the frontend codegens from.

The snapshot and contract test are not shipped yet. When added, refresh them
with [`data-raw/codegen.R`](../../data-raw/codegen.R). Targets API 0.2.x.
