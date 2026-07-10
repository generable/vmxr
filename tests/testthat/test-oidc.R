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

  # Secret cache must be owner-only. POSIX file modes only exist on unix; on
  # Windows file.info()$mode reflects the read-only bit, not 0600, and the
  # deployment target (the Linux home PVC) is unix, so scope the assertion.
  if (.Platform$OS.type != "windows") {
    expect_equal(as.character(file.info(cache)$mode), "600")
  }
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
  # The token is resolved per request via the provider, not frozen on the object.
  expect_equal(con$token_provider(), "cached_acc")
})

test_that("explicit token / PAT bypasses OIDC entirely", {
  cache <- withr::local_tempfile(fileext = ".json") # never written
  local_oidc_env(cache)
  con <- vmx_client(token = "pat_abc")
  expect_equal(con$token_provider(), "pat_abc")
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

# --- GEN-2344: persistent-client self-heal + refresh review nits --------------

test_that("a long-lived vmx_client() refreshes a stale access token on a later call", {
  # The structural gap the rest of the suite can't catch: it only exercises the
  # fresh-client-per-call path. Here one client is reused across a token expiry.
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  # A valid (non-expired) token at construction time.
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_valid", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 600,
      token_type = "Bearer", issuer = issuer, client_id = "test-cli"
    ),
    cache
  )
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token")
  )

  # Construct once (resolves the valid token; no network needed).
  con <- vmx_client()

  # Time passes and the access token expires. The provider re-reads the cache
  # each request, so rewriting it as expired simulates the wall-clock elapse.
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_valid", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) - 10,
      token_type = "Bearer", issuer = issuer, client_id = "test-cli"
    ),
    cache
  )

  # The next call on the *same* client refreshes (1st mock) then succeeds (2nd).
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(access_token = "acc_fresh", refresh_token = "ref2", expires_in = 600)),
    httr2::response_json(body = list(
      user_id = "usr_1", email = "a@b.co", name = "Ada", workspace_id = "ws_1",
      roles = list("admin"), counts = list(treatments = 0L, data_versions = 0L, model_fits = 0L)
    ))
  ))

  me <- vmx_whoami(con)
  expect_equal(me$email, "a@b.co")
  # The stale token was silently refreshed and re-cached -- no re-login.
  expect_equal(.vmx_load_cached_token(cache)$access_token, "acc_fresh")
})

test_that("a valid non-expired cached token is returned without refreshing (skew boundary)", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  # Life well beyond the 60s skew -> must be used as-is, not refreshed.
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_ok", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 600,
      token_type = "Bearer", issuer = issuer, client_id = "test-cli"
    ),
    cache
  )
  refreshed <- FALSE
  testthat::local_mocked_bindings(
    .vmx_oidc_refresh_flow = function(config, token) {
      refreshed <<- TRUE
      stop("refresh must not run for a still-valid token")
    }
  )
  expect_equal(vmx_oidc_access_token(can_prompt = FALSE), "acc_ok")
  expect_false(refreshed)
})

test_that("a cached token inside the expiry skew window is refreshed", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  # 30s of life left, inside the 60s skew -> treated as expired, refreshed early.
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_soon", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 30,
      token_type = "Bearer", issuer = issuer, client_id = "test-cli"
    ),
    cache
  )
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token")
  )
  httr2::local_mocked_responses(list(
    httr2::response_json(body = list(access_token = "acc_new", refresh_token = "ref2", expires_in = 600))
  ))
  expect_equal(vmx_oidc_access_token(can_prompt = FALSE), "acc_new")
})

test_that("a config-mismatch cache is reported (not silently reused) when non-interactive", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache) # config client_id = "test-cli"
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_other", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 600,
      token_type = "Bearer", issuer = issuer, client_id = "other-cli"
    ),
    cache
  )
  err <- tryCatch(vmx_oidc_access_token(can_prompt = FALSE), vmx_auth_error = function(e) e)
  expect_s3_class(err, "vmx_auth_error")
  expect_match(conditionMessage(err), "different issuer/client_id")
  # The mismatched token was never handed back.
  expect_false(grepl("acc_other", conditionMessage(err)))
})

test_that("a config-mismatch cache warns before overwriting on interactive login", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  .vmx_save_cached_token(
    .vmx_token(
      access_token = "acc_other", refresh_token = "ref",
      expires_at = as.numeric(Sys.time()) + 600,
      token_type = "Bearer", issuer = issuer, client_id = "other-cli"
    ),
    cache
  )
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token"),
    .vmx_oidc_device_flow = function(oauth_client, device_url, scopes) {
      list(access_token = "acc_relogin", refresh_token = "ref_new", expires_in = 600, token_type = "Bearer")
    }
  )
  expect_warning(
    access <- vmx_oidc_access_token(can_prompt = TRUE),
    "different provider"
  )
  expect_equal(access, "acc_relogin")
  # The cache was overwritten with the newly-issued (matching-config) token.
  expect_equal(.vmx_load_cached_token(cache)$client_id, "test-cli")
})

test_that("device-flow failures are wrapped as vmx_auth_error", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token"),
    .vmx_oidc_device_flow = function(oauth_client, device_url, scopes) {
      stop("device authorization was denied by the user")
    }
  )
  err <- tryCatch(vmx_login(), vmx_auth_error = function(e) e)
  expect_s3_class(err, "vmx_auth_error")
  expect_match(conditionMessage(err), "device-code login failed")
})

# -- GEN-2378: device-code prompt wording ------------------------------------
# httr2::oauth_flow_device() hard-codes a "Copy <code> and paste when requested"
# line, but it opens the pre-filled verification_uri_complete, so no paste ever
# happens. .vmx_oidc_device_prompt() pre-frames that as verify-and-approve in the
# interactive/browser branch, and stays silent in the non-interactive branch
# (where httr2's own "Visit <url> and enter code" instruction is accurate).

test_that("device prompt reframes the browser flow as verify-and-approve (GEN-2378)", {
  # Widen cli + collapse whitespace so console line-wrapping can't split a phrase
  # mid-word and break the substring matches below.
  withr::local_options(cli.width = 10000)
  out <- gsub("\\s+", " ", paste(cli::cli_fmt(.vmx_oidc_device_prompt(interactive = TRUE)), collapse = " "))
  # Tells the user to verify the code matches and approve -- not to paste it.
  expect_match(out, "matches", ignore.case = TRUE)
  expect_match(out, "approve", ignore.case = TRUE)
  # The modified note: warn that no paste/enter prompt may appear (code pre-filled).
  expect_match(out, "pre-filled", ignore.case = TRUE)
  expect_match(out, "may not be prompted", ignore.case = TRUE)
  # It must NOT instruct copy-and-paste (the misleading step this issue removes).
  expect_false(grepl("paste when requested", out, ignore.case = TRUE))
})

test_that("device prompt stays silent in the non-interactive fallback branch (GEN-2378)", {
  # httr2 prints "Visit <url> and enter code <code>" there, which is accurate, so
  # vmxr adds nothing (the paste/enter instruction belongs only on that branch).
  expect_identical(cli::cli_fmt(.vmx_oidc_device_prompt(interactive = FALSE)), character(0))
})

test_that("vmx_login shows the reframed device guidance before running the flow (GEN-2378)", {
  cache <- withr::local_tempfile(fileext = ".json")
  local_oidc_env(cache)
  seen <- character(0)
  testthat::local_mocked_bindings(
    .vmx_oidc_discover = function(config) list(device = "https://auth.test/device", token = "https://auth.test/token"),
    # Record ordering: the guidance must precede the (browser-opening) flow.
    .vmx_oidc_device_prompt = function(interactive = rlang::is_interactive()) {
      seen <<- c(seen, "prompt")
      invisible()
    },
    .vmx_oidc_device_flow = function(oauth_client, device_url, scopes) {
      seen <<- c(seen, "flow")
      list(access_token = "acc", refresh_token = "ref", expires_in = 600, token_type = "Bearer")
    }
  )
  suppressMessages(vmx_login())
  expect_identical(seen, c("prompt", "flow"))
})
