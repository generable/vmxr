# Unit tests for native OIDC device-code auth (GEN-2332). The device-code flow
# and provider I/O are stubbed (mocked bindings + httr2 response mocks) so no
# live Authentik / browser is needed.

issuer <- "https://auth.test/application/o/vmx-cli/"
stripped_issuer <- "https://auth.test/application/o/vmx-cli"

local_oidc_env <- function(cache, env = parent.frame()) {
  withr::local_envvar(
    .new = list(
      VMX_OIDC_ISSUER = issuer,
      VMX_OIDC_CLIENT_ID = "test-cli",
      VMX_OIDC_SCOPES = "",
      VMX_OIDC_TOKEN_CACHE = cache,
      VMX_API_TOKEN = "",
      VMX_API_BASE_URL = "https://vmx.test"
    ),
    .local_envir = env
  )
}

test_that("vmx_login runs the device flow and caches a CLI-shaped token 0600", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token"),
    .vmx_oidc_device_flow = function(oauth_client, device_url, scopes) {
      list(access_token = "acc_123", refresh_token = "ref_456", expires_in = 600, token_type = "Bearer")
    }
  )

  tok <- vmx_login()
  expect_equal(tok$access_token, "acc_123")
  expect_equal(tok$refresh_token, "ref_456")
  expect_equal(tok$issuer, stripped_issuer) # trailing slash stripped, matches CLI
  expect_true(file.exists(cache))

  # On-disk shape matches the vmx-cli cache (clients/cli/.../oidc.py TokenSet).
  on_disk <- jsonlite::fromJSON(cache, simplifyVector = TRUE)
  expect_setequal(
    names(on_disk),
    c("access_token", "refresh_token", "expires_at", "token_type", "issuer", "client_id")
  )
  expect_equal(on_disk$client_id, "test-cli")

  # Secret cache must be owner-only.
  expect_equal(as.character(file.info(cache)$mode), "600")
})

test_that("default scopes request offline_access (needed for a refresh token)", {
  withr::local_envvar(VMX_OIDC_ISSUER = issuer, VMX_OIDC_CLIENT_ID = "test-cli", VMX_OIDC_SCOPES = "")
  cfg <- vmx_oidc_config()
  expect_match(cfg$scopes, "offline_access")
  expect_match(cfg$scopes, "goauthentik.io/api")
})

test_that("cache survives a fresh session / pod restart (round-trips through disk)", {
  cache <- withr::local_tempfile(fileext = ".json")
  token <- .vmx_token(
    access_token = "acc", refresh_token = "ref",
    expires_at = as.numeric(Sys.time()) + 600,
    token_type = "Bearer", issuer = issuer, client_id = "test-cli"
  )
  .vmx_save_cached_token(token, cache)

  # A brand-new reader (simulating a fresh R process) sees the same token.
  reloaded <- .vmx_load_cached_token(cache)
  expect_equal(reloaded$access_token, "acc")
  expect_equal(reloaded$refresh_token, "ref")
  expect_equal(reloaded$issuer, stripped_issuer)
})

test_that("a corrupt cache is treated as no token, not an error", {
  cache <- withr::local_tempfile(fileext = ".json")
  writeLines("{ not json", cache)
  expect_null(.vmx_load_cached_token(cache))
})

test_that("expired cached token is silently refreshed and re-cached", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  expired <- .vmx_token(
    access_token = "old_acc", refresh_token = "ref_old",
    expires_at = as.numeric(Sys.time()) - 10, # already expired
    token_type = "Bearer", issuer = issuer, client_id = "test-cli"
  )
  .vmx_save_cached_token(expired, cache)

  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token")
  )
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(
      access_token = "new_acc", refresh_token = "ref_new", expires_in = 600, token_type = "Bearer"
    ))
  ))

  access <- vmx_oidc_access_token(can_prompt = FALSE)
  expect_equal(access, "new_acc")
  # Re-cached with the refreshed token.
  expect_equal(.vmx_load_cached_token(cache)$access_token, "new_acc")
  expect_equal(.vmx_load_cached_token(cache)$refresh_token, "ref_new")
})

test_that("refresh keeps the prior refresh token when the provider omits a new one", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  expired <- .vmx_token(
    access_token = "old_acc", refresh_token = "ref_keep",
    expires_at = as.numeric(Sys.time()) - 10,
    token_type = "Bearer", issuer = issuer, client_id = "test-cli"
  )
  .vmx_save_cached_token(expired, cache)

  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token")
  )
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(access_token = "new_acc", expires_in = 600)) # no refresh_token
  ))

  access <- vmx_oidc_access_token(can_prompt = FALSE)
  expect_equal(access, "new_acc")
  expect_equal(.vmx_load_cached_token(cache)$refresh_token, "ref_keep")
})

test_that("vmx_client auto-authenticates from a valid cached OIDC token", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "cached_acc", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 600,
      token_type = "Bearer", issuer = issuer, client_id = "test-cli"
    ),
    cache
  )

  con <- vmx_client()
  expect_s3_class(con, "vmx_client")
  expect_equal(con$token, "cached_acc")
})

test_that("explicit token / PAT bypasses OIDC entirely", {
  cache <- withr::local_tempfile(fileext = ".json") # never written
  local_oidc_env(cache)
  con <- vmx_client(token = "pat_abc")
  expect_equal(con$token, "pat_abc")
  expect_false(file.exists(cache))
})

test_that("no PAT and no OIDC config raises vmx_auth_error naming both methods", {
  withr::local_envvar(
    VMX_API_TOKEN = "", VMX_OIDC_ISSUER = "", VMX_OIDC_CLIENT_ID = "",
    VMX_API_BASE_URL = "https://vmx.test"
  )
  expect_error(vmx_client(), class = "vmx_auth_error")
})

test_that("configured OIDC but no cache and non-interactive raises vmx_auth_error", {
  cache <- withr::local_tempfile(fileext = ".json") # absent
  local_oidc_env(cache)
  expect_error(vmx_oidc_access_token(can_prompt = FALSE), class = "vmx_auth_error")
})
