# Native OIDC device-code authentication (GEN-2332).
#
# vmxr authenticates to vmx-api entirely in R via the OIDC device-code flow
# (RFC 8628, run by httr2::oauth_flow_device) against the staging Authentik
# vmx-cli provider -- no Python CLI dependency. `vmx_login()` runs the flow and
# caches the token; `vmx_client()` auto-authenticates from that cache, silently
# refreshing with the refresh token, and prompting a fresh login only when there
# is no usable cached token.
#
# The on-disk cache is a plain JSON file with the *exact same shape* the vmx-cli
# writes (clients/cli/src/vmx_cli/oidc.py: TokenSet.to_json), stored at the CLI's
# path `~/.config/vmx/oidc-token.json` on the persistent home PVC. That makes one
# `vmx_login()` serve both R and the terminal CLI, and lets the token survive a
# fresh R session / pod restart. Config comes from the same env vars the CLI
# reads: VMX_OIDC_ISSUER / VMX_OIDC_CLIENT_ID / VMX_OIDC_SCOPES.

# Default scopes. `offline_access` is required to be issued a refresh token
# (Authentik grants it natively even though it is absent from discovery
# `scopes_supported` -- confirmed by the GEN-2330 spike). `goauthentik.io/api`
# is required or vmx-api's Bearer shim (auth.py:_from_bearer -> Authentik
# /api/v3/core/users/me/) 401s on every call.
.vmx_oidc_default_scopes <- "openid profile email offline_access goauthentik.io/api"

# The CLI's cache path (sibling of the CLI login-config under ~/.config/vmx).
.vmx_oidc_default_cache <- "~/.config/vmx/oidc-token.json"

# Re-mint a cached access token this many seconds before its real expiry, so a
# token is never used past its life mid-request (matches the CLI's skew).
.vmx_oidc_expiry_skew <- 60

.vmx_oidc_user_agent <- "vmxr (https://github.com/generable/vmxr)"

# -- config -------------------------------------------------------------------

#' Resolve OIDC client config from args / env vars
#'
#' Precedence per field: explicit arg -> env var -> (scopes only) the default
#' scopes. The issuer is stripped of trailing slashes so it matches the CLI's
#' `issuer.rstrip('/')` in the shared cache. Missing issuer / client_id is a
#' usage error.
#' @keywords internal
#' @noRd
vmx_oidc_config <- function(issuer = NULL, client_id = NULL, scopes = NULL) {
  issuer <- trimws(issuer %||% Sys.getenv("VMX_OIDC_ISSUER", unset = ""))
  client_id <- trimws(client_id %||% Sys.getenv("VMX_OIDC_CLIENT_ID", unset = ""))
  scopes <- trimws(scopes %||% Sys.getenv("VMX_OIDC_SCOPES", unset = ""))
  if (!nzchar(scopes)) scopes <- .vmx_oidc_default_scopes

  if (!nzchar(issuer)) {
    vmx_abort(
      "No OIDC issuer. Set `issuer=` or the VMX_OIDC_ISSUER env var.",
      class = "vmx_usage_error"
    )
  }
  if (!nzchar(client_id)) {
    vmx_abort(
      "No OIDC client id. Set `client_id=` or the VMX_OIDC_CLIENT_ID env var.",
      class = "vmx_usage_error"
    )
  }

  list(issuer = sub("/+$", "", issuer), client_id = client_id, scopes = scopes)
}

# TRUE when both required OIDC env vars are set (so vmx_client() can decide
# whether an auto-auth attempt is even possible).
.vmx_oidc_configured <- function() {
  nzchar(trimws(Sys.getenv("VMX_OIDC_ISSUER", unset = ""))) &&
    nzchar(trimws(Sys.getenv("VMX_OIDC_CLIENT_ID", unset = "")))
}

# -- token value --------------------------------------------------------------

# A cached token: a plain list mirroring the CLI's on-disk fields. `expires_at`
# is an absolute epoch-seconds deadline; `refresh_token` is NULL when absent.
.vmx_token <- function(access_token, refresh_token, expires_at, token_type,
                       issuer, client_id) {
  list(
    access_token = access_token,
    refresh_token = if (is.null(refresh_token) || !nzchar(refresh_token)) NULL else refresh_token,
    expires_at = as.numeric(expires_at),
    token_type = token_type %||% "Bearer",
    issuer = sub("/+$", "", issuer),
    client_id = client_id
  )
}

.vmx_token_expired <- function(token, now = as.numeric(Sys.time()),
                               skew = .vmx_oidc_expiry_skew) {
  now >= (token$expires_at - skew)
}

.vmx_token_matches <- function(token, config) {
  identical(token$issuer, sub("/+$", "", config$issuer)) &&
    identical(token$client_id, config$client_id)
}

# Build a token from a raw OAuth token-endpoint response body.
.vmx_token_from_body <- function(config, body) {
  access <- body$access_token
  if (!is.character(access) || !length(access) || !nzchar(access)) {
    vmx_abort("OIDC token response did not contain an access_token.", class = "vmx_auth_error")
  }
  ttl <- suppressWarnings(as.numeric(body$expires_in %||% 300))
  if (is.na(ttl)) ttl <- 300
  .vmx_token(
    access_token = access,
    refresh_token = body$refresh_token,
    expires_at = as.numeric(Sys.time()) + ttl,
    token_type = body$token_type %||% "Bearer",
    issuer = config$issuer,
    client_id = config$client_id
  )
}

# Build a token from the httr2_token object oauth_flow_device() returns.
.vmx_token_from_httr2 <- function(config, token) {
  access <- token$access_token
  if (is.null(access) || !nzchar(access)) {
    vmx_abort("OIDC device-code flow returned no access token.", class = "vmx_auth_error")
  }
  expires_at <- token$expires_at
  if (is.null(expires_at)) {
    issued <- as.numeric(token$.date %||% Sys.time())
    ttl <- suppressWarnings(as.numeric(token$expires_in %||% 300))
    if (is.na(ttl)) ttl <- 300
    expires_at <- issued + ttl
  }
  .vmx_token(
    access_token = access,
    refresh_token = token$refresh_token,
    expires_at = expires_at,
    token_type = token$token_type %||% "Bearer",
    issuer = config$issuer,
    client_id = config$client_id
  )
}

# -- on-disk cache (CLI-compatible JSON, 0600) --------------------------------

.vmx_oidc_cache_path <- function() {
  path <- trimws(Sys.getenv("VMX_OIDC_TOKEN_CACHE", unset = ""))
  if (!nzchar(path)) path <- .vmx_oidc_default_cache
  path.expand(path)
}

# Serialize to the CLI's JSON shape (missing refresh token -> explicit null).
.vmx_token_json <- function(token) {
  jsonlite::toJSON(
    list(
      access_token = token$access_token,
      refresh_token = token$refresh_token %||% NA,
      expires_at = token$expires_at,
      token_type = token$token_type,
      issuer = token$issuer,
      client_id = token$client_id
    ),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
  )
}

.vmx_token_from_json <- function(data) {
  need <- c("access_token", "expires_at", "issuer", "client_id")
  if (!all(need %in% names(data))) stop("incomplete token cache", call. = FALSE)
  .vmx_token(
    access_token = as.character(data[["access_token"]]),
    refresh_token = {
      rt <- data[["refresh_token"]]
      if (is.null(rt) || !length(rt) || is.na(rt[1])) NULL else as.character(rt[1])
    },
    expires_at = as.numeric(data[["expires_at"]]),
    token_type = as.character(data[["token_type"]] %||% "Bearer"),
    issuer = as.character(data[["issuer"]]),
    client_id = as.character(data[["client_id"]])
  )
}

# Load the cached token, or NULL if absent/corrupt. A corrupt cache is treated
# as "no token" (re-login), not a hard error -- it is operator-local state.
.vmx_load_cached_token <- function(path = .vmx_oidc_cache_path()) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    .vmx_token_from_json(jsonlite::fromJSON(path, simplifyVector = TRUE)),
    error = function(e) NULL
  )
}

# Persist the token with owner-only (0600) perms. The cache holds a bearer +
# refresh token, so it must never be group/world readable: create the temp file
# 0600 *before* any secret bytes land, then atomically replace the target.
.vmx_save_cached_token <- function(token, path = .vmx_oidc_cache_path()) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  tmp <- paste0(path, ".tmp")
  file.create(tmp, showWarnings = FALSE)
  Sys.chmod(tmp, mode = "0600", use_umask = FALSE)
  writeLines(.vmx_token_json(token), tmp)
  file.rename(tmp, path)
  Sys.chmod(path, mode = "0600", use_umask = FALSE)
  invisible(path)
}

# -- provider I/O (discovery / device flow / refresh) -------------------------

.vmx_oidc_perform <- function(req, what) {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      vmx_abort(
        sprintf("OIDC %s request failed: %s", what, conditionMessage(e)),
        class = "vmx_auth_error", parent = e
      )
    }
  )
}

.vmx_oidc_error_detail <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(body)) {
    err <- body$error
    desc <- body$error_description
    if (!is.null(err) && !is.null(desc)) return(paste0(err, ": ", desc))
    if (!is.null(err)) return(as.character(err))
  }
  httr2::resp_status_desc(resp)
}

# Resolve device / token endpoints from the issuer's discovery document rather
# than hard-coding them (a provider-path change in Authentik then can't silently
# break vmxr). Matches the CLI's discovery.
.vmx_oidc_discover <- function(config) {
  url <- paste0(config$issuer, "/.well-known/openid-configuration")
  req <- httr2::request(url) |>
    httr2::req_user_agent(.vmx_oidc_user_agent) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- .vmx_oidc_perform(req, "discovery")
  if (httr2::resp_status(resp) >= 400) {
    vmx_abort(
      sprintf("OIDC discovery failed (HTTP %d) at %s", httr2::resp_status(resp), url),
      class = "vmx_auth_error"
    )
  }
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  list(
    device = .vmx_oidc_require_endpoint(body, "device_authorization_endpoint", "device-authorization endpoint"),
    token = .vmx_oidc_require_endpoint(body, "token_endpoint", "token endpoint")
  )
}

.vmx_oidc_require_endpoint <- function(discovery, key, what) {
  endpoint <- discovery[[key]]
  if (!is.character(endpoint) || length(endpoint) != 1L || !nzchar(endpoint)) {
    vmx_abort(
      sprintf(
        "The OIDC provider does not advertise a %s ('%s' missing from discovery).",
        what, key
      ),
      class = "vmx_auth_error"
    )
  }
  endpoint
}

# A public OAuth client: client_id in the request body, no secret, no PKCE.
.vmx_oidc_client <- function(config, token_endpoint) {
  httr2::oauth_client(id = config$client_id, token_url = token_endpoint, auth = "body")
}

# Wrap httr2's RFC 8628 device-code flow in a mockable binding. Kept minimal so
# tests can stub it (the real flow opens a browser and blocks on polling).
.vmx_oidc_device_flow <- function(oauth_client, device_url, scopes) {
  httr2::oauth_flow_device(oauth_client, auth_url = device_url, scope = scopes)
}

# Exchange a refresh token for a fresh access token (no browser). Authentik may
# omit a new refresh token on refresh (non-rotating), so the caller keeps the
# prior one.
.vmx_oidc_refresh <- function(config, token_endpoint, refresh_token) {
  req <- httr2::request(token_endpoint) |>
    httr2::req_user_agent(.vmx_oidc_user_agent) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_form(
      grant_type = "refresh_token",
      refresh_token = refresh_token,
      client_id = config$client_id,
      scope = config$scopes
    )
  resp <- .vmx_oidc_perform(req, "refresh")
  if (httr2::resp_status(resp) >= 400) {
    vmx_abort(
      paste0("OIDC refresh token was rejected: ", .vmx_oidc_error_detail(resp)),
      class = "vmx_auth_error"
    )
  }
  .vmx_token_from_body(config, httr2::resp_body_json(resp, simplifyVector = FALSE))
}

.vmx_oidc_refresh_flow <- function(config, token) {
  endpoints <- .vmx_oidc_discover(config)
  refreshed <- .vmx_oidc_refresh(config, endpoints$token, token$refresh_token)
  if (is.null(refreshed$refresh_token)) refreshed$refresh_token <- token$refresh_token
  refreshed
}

# -- public entry point + client integration ----------------------------------

#' Authenticate to VeloMetrix via OIDC device-code
#'
#' Runs the OIDC device-code flow (RFC 8628, via [httr2::oauth_flow_device()])
#' against the workspace's Authentik provider and caches the resulting token so
#' later [vmx_client()] calls authenticate automatically. Endpoints are resolved
#' from the issuer's `.well-known/openid-configuration` discovery document; the
#' flow uses a **public client** (no secret, no PKCE) and requests
#' `offline_access` so Authentik issues a **refresh token**.
#'
#' The token is written as plain JSON to `~/.config/vmx/oidc-token.json` (the
#' same path and shape the `vmx` CLI uses, with `0600` permissions), so one
#' `vmx_login()` serves both R and the terminal CLI and the session survives a
#' fresh R process or workspace pod restart. Because the refresh token is
#' persisted on the home PVC, you log in **once per refresh-token lifetime**
#' (~30 days).
#'
#' Configuration is read from environment variables (matching the CLI):
#' `VMX_OIDC_ISSUER`, `VMX_OIDC_CLIENT_ID`, and optionally `VMX_OIDC_SCOPES`.
#' Confirmed-working staging values: issuer
#' `https://auth.staging.gnrbl.co/application/o/generable-staging-vmx-cli/`,
#' client id `generable-staging-vmx-cli`.
#'
#' @param issuer OIDC issuer base URL. Defaults to `VMX_OIDC_ISSUER`.
#' @param client_id Public OIDC client id. Defaults to `VMX_OIDC_CLIENT_ID`.
#' @param scopes Space-separated scopes. Defaults to `VMX_OIDC_SCOPES`, then to
#'   `"openid profile email offline_access goauthentik.io/api"`.
#' @param cache_path Where to cache the token. Defaults to
#'   `VMX_OIDC_TOKEN_CACHE`, then `~/.config/vmx/oidc-token.json`.
#'
#' @return Invisibly, the cached token (a list; the access/refresh tokens are
#'   secret and never printed).
#' @export
vmx_login <- function(issuer = NULL, client_id = NULL, scopes = NULL,
                      cache_path = NULL) {
  config <- vmx_oidc_config(issuer, client_id, scopes)
  cache_path <- cache_path %||% .vmx_oidc_cache_path()
  endpoints <- .vmx_oidc_discover(config)
  oauth_client <- .vmx_oidc_client(config, endpoints$token)
  raw <- .vmx_oidc_device_flow(oauth_client, endpoints$device, config$scopes)
  token <- .vmx_token_from_httr2(config, raw)
  .vmx_save_cached_token(token, cache_path)
  cli::cli_alert_success("Logged in to VeloMetrix; token cached at {.path {cache_path}}.")
  invisible(token)
}

#' Resolve an access token for a client from the OIDC cache
#'
#' Returns a usable access token, refreshing silently when the cached one is
#' expired, and running [vmx_login()] when there is no usable cached token (only
#' possible in an interactive session; otherwise a `vmx_auth_error` is raised).
#' @keywords internal
#' @noRd
vmx_oidc_access_token <- function(config = vmx_oidc_config(),
                                  cache_path = .vmx_oidc_cache_path(),
                                  can_prompt = interactive()) {
  token <- .vmx_load_cached_token(cache_path)
  if (!is.null(token) && .vmx_token_matches(token, config)) {
    if (!.vmx_token_expired(token)) return(token$access_token)
    if (!is.null(token$refresh_token)) {
      refreshed <- tryCatch(.vmx_oidc_refresh_flow(config, token), error = function(e) NULL)
      if (!is.null(refreshed)) {
        .vmx_save_cached_token(refreshed, cache_path)
        return(refreshed$access_token)
      }
    }
  }
  if (!isTRUE(can_prompt)) {
    vmx_abort(
      paste0(
        "No usable cached OIDC token and not in an interactive session. Run ",
        "`vmx_login()` interactively (or set `token=` / VMX_API_TOKEN)."
      ),
      class = "vmx_auth_error"
    )
  }
  token <- vmx_login(
    issuer = config$issuer, client_id = config$client_id,
    scopes = config$scopes, cache_path = cache_path
  )
  token$access_token
}

# Resolve a bearer token for vmx_client() when no PAT is configured. Attempts
# OIDC auto-auth when the OIDC env vars are set; otherwise raises a single
# vmx_auth_error naming both auth methods.
.vmx_client_bearer <- function() {
  if (!.vmx_oidc_configured()) {
    vmx_abort(
      paste0(
        "No API token. Set `token=` / VMX_API_TOKEN (an Authentik PAT), or ",
        "configure OIDC (VMX_OIDC_ISSUER + VMX_OIDC_CLIENT_ID) and run `vmx_login()`."
      ),
      class = "vmx_auth_error"
    )
  }
  vmx_oidc_access_token()
}
