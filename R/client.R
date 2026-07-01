# Connection object: base_url / token resolution.

#' Create a vmxr client
#'
#' Resolves connection config with the precedence: explicit args ->
#' `VMX_API_BASE_URL` / `VMX_API_TOKEN` environment variables -> `~/.Renviron`
#' -> a classed error. The token is stored on the object but redacted in
#' [print()] and never logged.
#'
#' @param base_url API base URL. Defaults to `Sys.getenv("VMX_API_BASE_URL")`.
#' @param token Authentik personal access token (PAT). Defaults to
#'   `Sys.getenv("VMX_API_TOKEN")`. Never hard-code a PAT in source.
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
  if (!nzchar(token)) {
    vmx_abort(
      "No API token. Set `token=` or the VMX_API_TOKEN env var.",
      class = "vmx_auth_error"
    )
  }

  structure(
    list(
      base_url = sub("/+$", "", base_url),
      token = token,
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
