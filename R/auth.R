# PAT resolution and identity probes.

#' Confirm the configured PAT and report identity
#'
#' Calls `GET /me`.
#'
#' @param client A `vmx_client`.
#' @return A `vmx_me` object: `user_id`, `email`, `name`, `workspace_id`,
#'   `roles`, and `counts`.
#' @export
vmx_whoami <- function(client = vmx_client()) {
  new_vmx_resource(vmx_get(client, "/me"), "vmx_me", "user_id")
}

#' Connectivity probe
#'
#' Calls `GET /health`.
#'
#' @param client A `vmx_client`.
#' @return Invisibly, a `vmx_health` object (`status`, `version`,
#'   `api_contract_version`, `engine`, `engine_version`); raises a classed error
#'   if the API is unreachable or unhealthy.
#' @export
vmx_health <- function(client = vmx_client()) {
  invisible(new_vmx_resource(vmx_get(client, "/health"), "vmx_health", "version"))
}
