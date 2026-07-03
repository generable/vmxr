# Connection object: base_url / token resolution.

#' Create a vmxr client
#'
#' Resolves connection config with the precedence: explicit args ->
#' `VMX_API_BASE_URL` / `VMX_API_TOKEN` environment variables -> `~/.Renviron`
#' -> a classed error. The token is stored on the object but redacted in
#' [print()] and never logged.
#'
#' When no token is supplied (neither `token=` nor `VMX_API_TOKEN`), the client
#' **auto-authenticates** via OIDC: it reads the token cached by [vmx_login()]
#' (`~/.config/vmx/oidc-token.json`), refreshing it silently when expired, and
#' -- in an interactive session -- running [vmx_login()] if there is no usable
#' cached token. This requires `VMX_OIDC_ISSUER` + `VMX_OIDC_CLIENT_ID` to be
#' set; otherwise a `vmx_auth_error` names both auth methods.
#'
#' The bearer token is resolved **per request**, not frozen at construction: a
#' PAT is constant, but an OIDC access token is short-lived (~5-10 min), so a
#' long-lived `con <- vmx_client()` re-resolves each request and silently
#' refreshes from the cached refresh token when the access token is near expiry.
#' This lets a reused client keep working across a full analysis session without
#' re-login (GEN-2344).
#'
#' @param base_url API base URL. Defaults to `Sys.getenv("VMX_API_BASE_URL")`.
#' @param token Authentik personal access token (PAT). Defaults to
#'   `Sys.getenv("VMX_API_TOKEN")`, then to an OIDC access token (see
#'   [vmx_login()]). Never hard-code a PAT in source.
#' @param ... Reserved for future options (timeouts, retries, user agent).
#'
#' @return An object of class `vmx_client`.
#' @export
vmx_client <- function(base_url = NULL, token = NULL, ...) {
  base_url <- trimws(base_url %||% Sys.getenv("VMX_API_BASE_URL", unset = ""))
  token <- trimws(token %||% Sys.getenv("VMX_API_TOKEN", unset = ""))

  if (!nzchar(base_url)) {
    vmx_abort(
      "No API base URL. Set `base_url=` or the VMX_API_BASE_URL env var.",
      class = "vmx_usage_error"
    )
  }
  # Resolve the bearer token via a *provider closure* re-invoked on every request
  # (see vmx_req), not a single string baked in here. A frozen OIDC access token
  # expires a few minutes into a persistent `con` and every later call then 401s
  # even though a valid refresh token is cached (GEN-2344); a provider re-reads
  # the cache and refreshes as needed, so the client self-heals.
  if (nzchar(token)) {
    # Explicit PAT / VMX_API_TOKEN: constant provider.
    pat <- token
    token_provider <- function() pat
  } else {
    # No PAT: OIDC device-code auth (cache -> refresh -> login), re-resolved
    # per request so an expired access token self-heals from the refresh token.
    token_provider <- .vmx_client_bearer_provider()
  }

  # Resolve once up front so auth problems (no cache, revoked refresh token, no
  # OIDC config) surface at construction rather than on the first API call, and
  # so the OIDC cache is primed. The return value is intentionally discarded --
  # requests re-resolve through the provider.
  token_provider()

  structure(
    list(
      base_url = sub("/+$", "", base_url),
      token_provider = token_provider,
      options = list(...)
    ),
    class = "vmx_client"
  )
}

#' @export
print.vmx_client <- function(x, ...) {
  cli::cli_text("<vmx_client>")
  cli::cli_bullets(c(
    "*" = "base_url: {.url {x$base_url}}",
    "*" = "token: {.field <redacted>}"
  ))
  invisible(x)
}
